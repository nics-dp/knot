(ns knot.prose
  "本文（ProseMirror JSON 的 EDN 形式）與 agent 交回的純文字格式互轉（SPEC 5.4「交回格式」）。
  doc 的 content 是 block 的序列：paragraph、bulletList、orderedList；清單只有一層，
  listItem 只含一個 paragraph。inline 節點為 text、fact（attrs {:key}）、ref（attrs {:label}）、
  cite（attrs {:block \"block id\"}）。
  純文字格式：每一行一段（空行忽略）；「- 」「* 」「• 」開頭的連續行是項目清單，
  「1. 」這類數字加句點開頭的連續行是編號清單（編號依順序產生）；
  標記為 {{fact:key}}、{{ref:label}}、[[cite:block-id]]。"
  (:require [clojure.string :as str]
            [knot.sources :as sources]))

;; ---------------------------------------------------------------------------
;; 純文字 → doc

(def ^:private marker-re
  "所有 {{...}} 與 [[...]] 形式的標記（不論內容是否合法）。"
  #"\{\{[^{}]*\}\}|\[\[[^\[\]]*\]\]")

(def ^:private bullet-re #"^[-*•]\s+(.+)$")
(def ^:private ordered-re #"^\d+\.\s+(.+)$")

(defn- marker->node
  "解析一個標記：回傳 inline 節點，或 {:error \"說明\"}。"
  [m]
  (let [curly? (str/starts-with? m "{{")
        inner (str/trim (subs m 2 (- (count m) 2)))
        idx (str/index-of inner ":")
        kind (when idx (str/trim (subs inner 0 idx)))
        value (when idx (str/trim (subs inner (inc idx))))
        bad #(hash-map :error (str "標記 " m " 格式不正確：" %))]
    (cond
      (nil? idx) (bad (if curly? "應為 {{fact:事實key}} 或 {{ref:label}}" "應為 [[cite:區塊id]]"))
      (and curly? (not (#{"fact" "ref"} kind))) (bad (str "{{ }} 只支援 fact 與 ref，不支援「" kind "」"))
      (and (not curly?) (not= "cite" kind)) (bad (str "[[ ]] 只支援 cite，不支援「" kind "」"))
      (str/blank? value) (bad (str kind " 後面沒有內容"))
      (re-find #"\s" value) (bad (str kind " 的內容不能含空白"))
      :else (case kind
              "fact" {:type "fact" :attrs {:key value}}
              "ref" {:type "ref" :attrs {:label value}}
              "cite" {:type "cite" :attrs {:block value}}))))

(defn- push-text [nodes s]
  (if (seq s) (conj nodes {:type "text" :text s}) nodes))

(defn- parse-inline
  "一行文字 → {:nodes [...] :errors [...]}。相鄰的 text 合併、不產生空字串 text；
  沒有配對的 {{ 或 [[ 也算錯誤。"
  [line]
  (loop [rest-s line, nodes [], errors []]
    (if-let [m (re-find marker-re rest-s)]
      (let [i (str/index-of rest-s m)
            before (subs rest-s 0 i)
            after (subs rest-s (+ i (count m)))
            node (marker->node m)
            nodes (push-text nodes before)]
        (recur after
               (if (:error node) nodes (conj nodes node))
               (cond-> errors (:error node) (conj (:error node)))))
      (let [nodes (push-text nodes rest-s)
            stray (some #(re-find #"\{\{|\[\[" %) (keep :text nodes))
            merged (reduce (fn [acc n]
                             (if (and (= "text" (:type n)) (= "text" (:type (peek acc))))
                               (conj (pop acc) (update (peek acc) :text str (:text n)))
                               (conj acc n)))
                           [] nodes)]
        {:nodes merged
         :errors (cond-> errors
                   stray (conj (str "標記沒有結尾：出現「" stray "」但找不到配對的結尾，標記要寫成 {{fact:key}}、{{ref:label}}、[[cite:區塊id]]")))}))))

(defn- classify
  "一行 → [:bullet 內容]、[:ordered 內容] 或 [:paragraph 內容]。"
  [line]
  (if-let [[_ t] (re-find bullet-re line)]
    [:bullet t]
    (if-let [[_ t] (re-find ordered-re line)]
      [:ordered t]
      [:paragraph line])))

(defn- refs-of [nodes type k]
  (->> nodes (filter #(= type (:type %))) (map #(get-in % [:attrs k])) distinct vec))

(defn markup->doc
  "純文字 → {:doc doc :refs {:facts [key...] :labels [label...] :blocks [block-id...]}}（依出現順序、去重）；
  格式錯誤時為 {:errors [說明...]}（一次列出全部）；全部空白也是錯誤。"
  [text]
  (let [lines (->> (str/split (or text "") #"\r?\n")
                   (map str/trim)
                   (remove str/blank?))]
    (if (empty? lines)
      {:errors ["本文是空白的：每一行是一段，至少要有一行內容"]}
      (let [parsed (for [l lines
                         :let [[kind t] (classify l)]]
                     (assoc (parse-inline t) :kind kind))
            errors (vec (distinct (mapcat :errors parsed)))]
        (if (seq errors)
          {:errors errors}
          (let [;; 連續同類的清單行併成一個清單
                groups (partition-by #(if (= :paragraph (:kind %)) (gensym) (:kind %)) parsed)
                para (fn [p] {:type "paragraph" :content (:nodes p)})
                blocks (vec (for [g groups
                                  :let [kind (:kind (first g))]]
                              (if (= :paragraph kind)
                                (para (first g))
                                {:type (if (= :bullet kind) "bulletList" "orderedList")
                                 :content (mapv (fn [p] {:type "listItem" :content [(para p)]}) g)})))
                inlines (mapcat :nodes parsed)]
            {:doc {:type "doc" :content blocks}
             :refs {:facts (refs-of inlines "fact" :key)
                    :labels (refs-of inlines "ref" :label)
                    :blocks (refs-of inlines "cite" :block)}}))))))

;; ---------------------------------------------------------------------------
;; doc → 純文字

(defn- inline->markup [{:keys [type text attrs]}]
  (case type
    "text" text
    "fact" (str "{{fact:" (:key attrs) "}}")
    "ref" (str "{{ref:" (:label attrs) "}}")
    "cite" (str "[[cite:" (:block attrs) "]]")
    ""))

(defn- render-blocks
  "每個 block 轉成行的序列；inline-fn 把 inline 節點轉成字串。空段落不輸出。"
  ([doc inline-fn] (render-blocks doc inline-fn true))
  ([doc inline-fn prefix?]
   (mapcat (fn [block]
            (let [para-line #(apply str (map inline-fn (:content %)))
                  item-lines (fn [prefix-fn]
                               (map-indexed (fn [i item]
                                              (str (prefix-fn i) (apply str (map para-line (:content item)))))
                                            (:content block)))]
              (case (:type block)
                "bulletList" (item-lines (constantly (if prefix? "- " "")))
                "orderedList" (item-lines #(if prefix? (str (inc %) ". ") ""))
                (let [l (para-line block)] (when-not (str/blank? l) [l])))))
          (:content doc))))

(defn doc->markup
  "doc → 純文字格式（markup->doc 的反向）。"
  [doc]
  (str/join "\n" (render-blocks doc inline->markup)))

(defn- display-inline [facts ref-fn {:keys [type text attrs]}]
  (case type
    "text" text
    "fact" (let [f (get facts (:key attrs))]
             (if (:fact/value f) (sources/fact-display f) (str "{" (:key attrs) "}")))
    "ref" (or (some-> ref-fn (#(% (:label attrs)))) (str "{" (:label attrs) "}"))
    ""))

(defn doc->text
  "doc → 顯示用純文字：事實換成值與單位（facts 為 key → 事實；查不到時為 {key}）、
  ref 顯示 {label}、cite 不輸出；清單項目前加「- 」或「1. 」。
  選填 ref-fn：label → 顯示字串或 nil；回傳 nil 時 ref 仍顯示 {label}。"
  ([facts doc] (doc->text facts doc nil))
  ([facts doc ref-fn]
   (str/join "\n" (render-blocks doc #(display-inline facts ref-fn %)))))

(def ^:private whitespace-re #"[\s\u3000\u00a0]+")

(defn char-count
  "本文的字數（SPEC 5.3「字數」）：顯示用文字（同 doc->text，事實代入值與單位）去掉所有空白字元
  （含全形空白、換行）後的字元數；清單的編號與符號不算。
  查不到的事實以「{key}」計（含大括號，與畫面顯示一致）；交互參照同理以「{label}」計；cite 不算。
  doc 為 nil 或沒有內容時為 0。以 UTF-16 單位計，罕用的擴充字集（每字 2 單位）會多算。"
  [facts doc]
  (->> (render-blocks doc #(display-inline facts nil %) false)
       (map #(count (str/replace % whitespace-re "")))
       (reduce + 0)))

;; ---------------------------------------------------------------------------
;; 未綁定的數字

(def ^:private number-re
  "數字（半形或全形）：可含千分位逗號、小數點，結尾可有 %；
  前面可緊接「第」（群組 1），後面可緊接年月日或節章項款（群組 3，中間可有空白）。"
  #"(第\s*)?([0-9０-９][0-9０-９,.．]*[%％]?)(\s*[年月日節章項款])?")

(def ^:private dotted-3-re #"^[0-9０-９]+([.．][0-9０-９]+){2,}$")
(def ^:private dotted-2-re #"^[0-9０-９]+[.．][0-9０-９]+$")

(defn- not-a-count?
  "年月日與章節編號不是需要綁定事實的數字：
  數字後面緊接年、月、日（例如「114 年」「3 月」「15 日」，民國與西元都一樣）；
  三段以上以句點分隔的整數（2.1.1）；兩段的數字（2.1）只有前面緊接「第」或後面緊接節章項款時才算編號，
  否則可能是小數（2.5），仍算數字。"
  [num prefix suffix]
  (let [unit (some-> suffix str/trim)]
    (boolean
     (or (#{"年" "月" "日"} unit)
         (re-find dotted-3-re num)
         (and (re-find dotted-2-re num)
              (or prefix (#{"節" "章" "項" "款"} unit)))))))

(defn- numbers-in-doc
  "本文中不在標記內的數字（只看 text 節點），依出現順序去重；
  結尾的逗號與句點視為標點，不算在數字內；年月日與章節編號不算。"
  [doc]
  (->> (tree-seq coll? seq doc)
       (filter #(and (map? %) (= "text" (:type %))))
       (mapcat #(re-seq number-re (:text %)))
       (keep (fn [[_ prefix num suffix]]
               (let [num (if (re-find #"[%％]$" num) num (str/replace num #"[,.．]+$" ""))]
                 (when-not (not-a-count? num prefix suffix) num))))
       distinct
       vec))

(defn unbound-numbers
  "本文中未綁定事實的數字（SPEC 5.4「驗證」）。facts 是報告的事實（key → 事實）：
  報告沒有任何事實時沒有東西可綁，不提醒，回傳 []。畫面與 MCP 共用這個函式。"
  [facts doc]
  (if (empty? facts)
    []
    (numbers-in-doc doc)))
