(ns knot.latex
  "LaTeX 匯出（SPEC 5.9）：由報告狀態產生 zip 內每個檔案的內容，全部是純函式。
  入口 `export-files`：state（:report :nodes :facts）→ {\"<目錄>/main.tex\" \"內容\" ...}；
  匯出時間由參數傳入，讓輸出可以逐字比對。排版一律用最簡單的寫法，只保證內容與插入關係正確。"
  (:require [clojure.string :as str]
            [knot.sources :as sources]
            [knot.tree :as tree]))

;; ---------------------------------------------------------------------------
;; 跳脫與目錄名稱

(def ^:private escapes
  {"\\" "\\textbackslash{}" "{" "\\{" "}" "\\}" "$" "\\$" "&" "\\&" "#" "\\#"
   "^" "\\textasciicircum{}" "_" "\\_" "%" "\\%" "~" "\\textasciitilde{}"})

(defn escape
  "跳脫 LaTeX 特殊字元；一次掃過整個字串，所以反斜線不會被二次跳脫。全形字元不處理。"
  [s]
  (str/replace (str s) #"[\\{}$&#^_%~]" #(get escapes (str %))))

(defn directory-name
  "報告名稱 → zip 內的目錄名稱：`/ \\ : * ? \" < > |` 與控制字元換成「_」，前後空白去掉；
  空白（或只有句點）時為「report」。"
  [title]
  (let [s (-> (str title)
              (str/replace #"[/\\:*?\"<>|\u0000-\u001f\u007f]" "_")
              str/trim)]
    (if (or (str/blank? s) (re-matches #"\.+" s)) "report" s)))

(defn- chapter-file-name [n]
  (str "ch" (if (< n 10) "0" "") n))

(defn- one-line [s] (str/trim (str/replace (str s) #"\s+" " ")))

;; ---------------------------------------------------------------------------
;; 本文

(defn- ref->latex [label]
  (cond
    (str/starts-with? label "tab:") (str "表~\\ref{" label "}")
    (str/starts-with? label "fig:") (str "圖~\\ref{" label "}")
    :else (str "\\ref{" label "}")))

(defn- inline->latex [facts {:keys [type text attrs]}]
  (case type
    "text" (escape text)
    "fact" (let [f (get facts (:key attrs))]
             (escape (if (:fact/value f) (sources/fact-display f) (str "{" (:key attrs) "}"))))
    "ref" (ref->latex (:label attrs))
    ""))

(defn- block->latex [facts block]
  (let [inl #(apply str (map (partial inline->latex facts) (:content %)))
        items (fn [env]
                (let [lines (for [item (:content block)
                                  :let [t (apply str (map inl (:content item)))]
                                  :when (not (str/blank? t))]
                              (str "  \\item " t))]
                  (when (seq lines)
                    (str "\\begin{" env "}\n" (str/join "\n" lines) "\n\\end{" env "}"))))]
    (case (:type block)
      "bulletList" (items "itemize")
      "orderedList" (items "enumerate")
      (let [t (inl block)] (when-not (str/blank? t) t)))))

(defn prose->latex
  "本文（ProseMirror doc）→ LaTeX：每段一行、段落之間空一行；清單為 itemize／enumerate。
  事實代入顯示值（含單位），ref 依 label 前綴輸出，cite 不輸出。沒有內容時為 nil。"
  [facts doc]
  (let [blocks (keep (partial block->latex facts) (:content doc))]
    (when (seq blocks) (str/join "\n\n" blocks))))

;; ---------------------------------------------------------------------------
;; 檔案內容

(defn- comment-lines
  "每一行加上「% 」。"
  [s]
  (str/join "\n" (map #(str/trimr (str "% " %)) (str/split-lines (str s)))))

(defn- unwritten [intent]
  (if (str/blank? intent)
    "% 尚未撰寫"
    (let [[l & more] (str/split-lines (str/trim intent))]
      (str/join "\n" (cons (str "% 尚未撰寫：" l) (map #(str/trimr (str "% " %)) more))))))

(defn- header [{:keys [title exported-at]}]
  (str "% 由 Knot 產生\n"
       "% 報告：" (one-line title) "\n"
       "% 匯出時間：" exported-at "\n"
       "% 重新匯出會覆蓋此檔，請改 main.tex 或在 Knot 修改"))

(defn- alias-of [node]
  (let [l (:node/label node)]
    (if l (str/replace l #"^(tab|fig):" "") (str (:node/id node)))))

(def ^:private heading-commands ["chapter" "section" "subsection" "subsubsection"])

(defn- body-or-todo [facts node]
  (or (prose->latex facts (:node/prose node)) (unwritten (:node/draft node))))

(defn- float->latex [node]
  (let [a (alias-of node) label (:node/label node)
        caption (str "\\caption{" (escape (:node/title node)) "}")
        label-line (when label (str "  \\label{" label "}"))]
    (if (= :table (:node/kind node))
      (str/join "\n" (concat ["\\begin{table}[htbp]" "  \\centering" (str "  " caption)]
                             (when label-line [label-line])
                             [(str "  \\input{tables/" a "}") "\\end{table}"]))
      (str/join "\n" (concat ["\\begin{figure}[htbp]" "  \\centering" (str "  \\input{images/" a "}") (str "  " caption)]
                             (when label-line [label-line])
                             ["\\end{figure}"])))))

(defn- node-blocks
  "一張卡片（含底下的卡片）依文件順序輸出的區塊。"
  [{:keys [nodes facts] :as ctx} id]
  (let [n (get nodes id)
        kids (mapcat #(node-blocks ctx %) (tree/child-ids nodes id))]
    (case (:node/kind n)
      :heading (cons (str "\\" (get heading-commands (dec (tree/depth nodes id)) "subsubsection")
                          "{" (escape (:node/name n)) "}")
                     kids)
      :intro [(body-or-todo facts n)]
      :paragraph (cons (str (comment-lines (one-line (:node/title n))) "\n" (body-or-todo facts n)) kids)
      (:table :figure) [(float->latex n)]
      [])))

(defn chapter-file
  "章（第一層標題 id）的 .tex 內容。"
  [state meta id]
  (str (header meta) "\n\n"
       (str/join "\n\n" (node-blocks state id))
       "\n"))

(defn- table-file [meta {:node/keys [table-content]}]
  (let [{:keys [columns rows]} table-content
        n (count columns)
        row #(str (str/join " & " (map escape (take n (concat % (repeat ""))))) " \\\\")]
    (str (header meta) "\n"
         (if (seq columns)
           (str "\\begin{tabular}{" (apply str (repeat n "l")) "}\n"
                "\\toprule\n" (row columns) "\n\\midrule\n"
                (apply str (map #(str (row %) "\n") rows))
                "\\bottomrule\n\\end{tabular}\n")
           "\\begin{tabular}{l}\n\\toprule\n（表格尚未填寫） \\\\\n\\bottomrule\n\\end{tabular}\n"))))

(defn- figure-file [meta numbers {:keys [nodes]} node]
  (let [a (alias-of node)
        pos (tree/position-label nodes numbers (:node/id node))]
    (str (header meta) "\n"
         "% 圖：" (one-line (:node/title node)) "\n"
         (when pos (str "% 位置：" pos "\n"))
         (when-not (str/blank? (:node/draft node))
           (str (comment-lines (str "意圖：" (str/trim (:node/draft node)))) "\n"))
         "% 放入 images/" a ".pdf、.png 或 .jpg；也可以把這個檔改成 TikZ 或 pgfplots 繪圖\n"
         "\\includegraphics[width=0.8\\linewidth]{images/" a "}\n")))

(defn- images-readme [meta numbers {:keys [nodes]} figures]
  (str "由 Knot 產生\n"
       "報告：" (one-line (:title meta)) "\n"
       "匯出時間：" (:exported-at meta) "\n"
       "重新匯出會覆蓋此檔\n\n"
       "Knot 不存圖檔。請把下列圖檔放進這個資料夾（副檔名為 .pdf、.png 或 .jpg 皆可），重新編譯即可；"
       "還沒放入時，編譯結果會以帶檔名的空框代替。\n\n"
       (if (seq figures)
         (str/join "\n"
                   (for [f figures
                         :let [a (alias-of f)
                               pos (tree/position-label nodes numbers (:node/id f))]]
                     (str a ".pdf（或 " a ".png、" a ".jpg）\n"
                          "  圖名稱：" (one-line (:node/title f)) "\n"
                          "  所在章節：" (or pos "（無）") "\n")))
         "（本報告沒有圖）\n")))

(def ^:private includegraphics-redefinition
  (str "% 圖檔找不到時畫帶檔名的空框，讓缺圖也能編譯\n"
       "\\let\\knotincludegraphics\\includegraphics\n"
       "\\renewcommand{\\includegraphics}[2][]{%\n"
       "  \\IfFileExists{#2.pdf}{\\knotincludegraphics[#1]{#2.pdf}}{%\n"
       "  \\IfFileExists{#2.png}{\\knotincludegraphics[#1]{#2.png}}{%\n"
       "  \\IfFileExists{#2.jpg}{\\knotincludegraphics[#1]{#2.jpg}}{%\n"
       "    \\fbox{\\parbox[c][4cm][c]{0.8\\linewidth}{\\centering 缺圖：\\texttt{\\detokenize{#2}}}}}}}}\n"))

(defn- main-file [{:keys [title subtitle exported-at]} chapters]
  (let [first-appendix (some #(when (:appendix? %) (:file %)) chapters)]
    (str "% 由 Knot 產生的範例主檔（報告：" (one-line title) "；匯出時間：" exported-at "）\n"
         "% 這只是範例，可以自由修改，或只取 contents/、tables/ 放進自己的專案；重新匯出會覆蓋此檔。\n"
         "% 請以 XeLaTeX 編譯（xelatex main.tex，需要編譯兩次才有目錄與交互參照）。\n"
         "\\documentclass[a4paper,12pt]{ctexrep}\n"
         "\\usepackage{booktabs}\n"
         "\\usepackage{graphicx}\n"
         "\\ctexset{\n"
         "  contentsname = {目錄},\n"
         "  figurename = {圖},\n"
         "  tablename = {表},\n"
         "  appendixname = {附錄},\n"
         "  chapter = {name = {第,章}, number = {\\chinese{chapter}}},\n"
         "}\n"
         "\\setcounter{secnumdepth}{3}\n"
         "\\setcounter{tocdepth}{3}\n"
         includegraphics-redefinition
         "\n"
         "\\title{" (escape (one-line title))
         (when-not (str/blank? subtitle) (str "\\\\[1ex]\\large " (escape (one-line subtitle))))
         "}\n"
         "\\author{}\n"
         "\\date{}\n"
         "\n"
         "\\begin{document}\n"
         "\\maketitle\n"
         "\\tableofcontents\n\n"
         (str/join "\n"
                   (for [{:keys [file]} chapters]
                     (str (when (= file first-appendix) "\\appendix\n")
                          "\\include{contents/" file "}")))
         "\n\\end{document}\n")))

;; ---------------------------------------------------------------------------
;; 入口

(defn export-files
  "整份報告的匯出內容：{\"<目錄>/main.tex\" 內容, ...}（路徑含目錄前綴）。
  state 需要 :report（:report/title、:report/subtitle）、:nodes、:facts；
  opts：{:exported-at \"匯出時間字串\"}。"
  [{:keys [report nodes] :as state} {:keys [exported-at]}]
  (let [title (:report/title report)
        dir (directory-name title)
        meta {:title title :subtitle (:report/subtitle report) :exported-at exported-at}
        numbers (tree/numbering nodes)
        tops (filterv #(tree/heading? nodes %) (tree/child-ids nodes nil))
        {app true body false} (group-by #(tree/appendix? nodes %) tops)
        chapters (concat
                  (map-indexed (fn [i id] {:id id :file (chapter-file-name (inc i))}) body)
                  (map-indexed (fn [i id] {:id id :file (str "app" (tree/appendix-letter i)) :appendix? true}) app))
        cards (map nodes (tree/document-order nodes))
        tables (filter #(= :table (:node/kind %)) cards)
        figures (filter #(= :figure (:node/kind %)) cards)
        path #(str dir "/" %)]
    (into {(path "main.tex") (main-file meta chapters)
           (path "images/README.txt") (images-readme meta numbers state figures)}
          (concat
           (for [{:keys [id file]} chapters]
             [(path (str "contents/" file ".tex")) (chapter-file state meta id)])
           (for [t tables]
             [(path (str "tables/" (alias-of t) ".tex")) (table-file meta t)])
           (for [f figures]
             [(path (str "images/" (alias-of f) ".tex")) (figure-file meta numbers state f)])))))
