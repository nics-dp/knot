(ns knot.context
  "生成某張卡片的本文時，AI 會讀到的內容（SPEC 5.4）。
  周邊卡片有本文就帶本文，沒有就帶意圖：批次起草時鄰段都還沒有本文，自然各段獨立生成；
  之後新增或修改一段時，已有的鄰段本文會帶進來銜接。
  編輯區的預覽與 M4 實際組裝 prompt 共用這份結果。"
  (:require [clojure.string :as str]
            [knot.tree :as tree]))

(def relation-label
  {:progression "遞進" :causal "因果" :contrast "對比" :example "舉例"
   :summarizes "摘要" :basis "依據" :echo "呼應"})

(def remote-section-title
  "本段對其他段落的不相鄰關係，在生成時各成一節。"
  {:summarizes "本段摘要的段落"
   :basis "本段依據的段落"
   :echo "本段呼應的段落"
   :contrast "本段對比的段落"})

(def remote-uses-prose
  "摘要、依據要用到目標段落的結果，帶本文；呼應、對比只帶意圖。"
  #{:summarizes :basis})

(def group-label
  "編輯區摘要列的分組，依顯示順序。"
  [[:upper "上層"] [:self "本段"] [:neighbors "鄰近"] [:children "下層"] [:relations "關係"] [:sources "素材"] [:style "文風"]])

(defn prose-text
  "本文（ProseMirror JSON 的 EDN 形式）轉成純文字；事實引用換成值與單位，交互參照換成 label。"
  [facts doc]
  (->> (:content doc)
       (map (fn [block]
              (apply str (for [{:keys [type text attrs]} (:content block)]
                           (case type
                             "text" text
                             "fact" (let [{:fact/keys [value unit]} (get facts (:key attrs))]
                                      (if value (str value unit) (str "{" (:key attrs) "}")))
                             "ref" (str "{" (:label attrs) "}")
                             "")))))
       (str/join "\n")))

(defn- entry [label text & {:as more}]
  (merge {:label label :text (or text "")} more))

(defn- section [group title entries]
  (when (seq entries)
    {:group group :title title :entries (vec entries)}))

(defn- intent-entry [nodes label id & more]
  (apply entry label (get-in nodes [id :node/draft]) :source :intent more))

(defn- content-entry
  "有本文時帶本文，否則帶意圖。"
  [{:keys [nodes facts]} label id & more]
  (let [text (some->> (get-in nodes [id :node/prose]) (prose-text facts))]
    (if (str/blank? text)
      (apply intent-entry nodes label id more)
      (apply entry label text :source :prose more))))

(defn- heading-path [nodes title id]
  (for [a (reverse (tree/ancestor-ids nodes id))
        :when (tree/heading? nodes a)]
    (intent-entry nodes (title a) a)))

(defn- cited-block-ids [nodes id]
  (set (map :block (get-in nodes [id :node/citations]))))

(defn- citations [{:keys [nodes blocks]} id]
  (let [by-id (into {} (map (juxt :block/id identity)) blocks)]
    (for [{:keys [block where]} (get-in nodes [id :node/citations])
          :let [b (by-id block)]
          :when b]
      (entry (str (if (= :table (:block/kind b)) "表 " "段 ") (:block/title b))
             (if (= :table (:block/kind b))
               (str "欄位：" (str/join "、" (:table/columns b))
                    "；" (count (:table/rows b)) " 列"
                    (when where (str "（" where "）")))
               (:block/text b))
             :warn (= :candidate (:block/status b))))))

(defn- cited-facts
  "只帶引用素材上的事實，控制提示詞長度。"
  [{:keys [nodes facts]} id]
  (let [cited (cited-block-ids nodes id)]
    (for [{:fact/keys [key label value unit block]} (sort-by :fact/key (vals facts))
          :when (cited block)]
      (entry label (str value unit) :key key))))

(defn- relation-text [rel]
  (cond
    (nil? rel) "未標註關係"
    (:relation/pending rel) (str (some-> (:relation/type rel) relation-label (str "，")) "待確認")
    :else (relation-label (:relation/type rel))))

(defn- paragraph-context [{:keys [nodes relations report] :as state} title id]
  (let [node (get nodes id)
        parent (tree/parent nodes id)
        intro (some-> parent (#(tree/intro-id nodes %)))
        prev (tree/prev-paragraph nodes id)
        nxt (let [n (tree/next-sibling nodes id)] (when (tree/paragraph? nodes n) n))
        {:keys [outgoing incoming]} (tree/remote-relations relations id)
        summarizers (for [r incoming :when (= :summarizes (:relation/type r))]
                      (:relation/from r))]
    (keep identity
          (concat
           [(section :style "文風" [(entry "文風設定" (:report/style-name report))])
            (section :upper "章節路徑" (heading-path nodes title id))
            (section :upper "所屬標題的引言"
                     (when intro
                       (let [e (content-entry state "引言" intro)]
                         (when-not (str/blank? (:text e)) [e]))))
            (section :self "本段" [(intent-entry nodes (:node/title node) id)])
            (section :self "下層表格與圖"
                     (for [k (tree/child-ids nodes id)]
                       (intent-entry nodes (str (get-in nodes [k :node/label]) " " (title k)) k)))
            (section :sources "依據" (citations state id))
            (section :sources "可用事實" (cited-facts state id))
            (section :neighbors "前一段"
                     (when prev
                       [(content-entry state (title prev) prev
                                       :relation (relation-text (tree/relation-between relations prev id)))]))
            (section :neighbors "下一段"
                     (when nxt
                       [(content-entry state (title nxt) nxt
                                       :relation (relation-text (tree/relation-between relations id nxt)))]))]
           (for [t tree/remote-relation-types]
             (section :relations (remote-section-title t)
                      (for [r outgoing :when (= t (:relation/type r))
                            :let [to (:relation/to r)]]
                        (if (remote-uses-prose t)
                          (content-entry state (title to) to)
                          (intent-entry nodes (title to) to)))))
           ;; 摘要本段的段落依賴本段，只帶意圖讓 AI 知道本段的結果會被誰彙整
           [(section :relations "摘要本段的段落"
                     (for [s summarizers] (intent-entry nodes (title s) s)))]))))

(defn- intro-context [{:keys [nodes report] :as state} title id]
  (let [heading (tree/parent nodes id)]
    (keep identity
          [(section :style "文風" [(entry "文風設定" (:report/style-name report))])
           (section :upper "章節路徑" (heading-path nodes title id))
           (section :self "引言的意圖" [(intent-entry nodes "引言" id)])
           (section :children "這一層的下層內容"
                    (for [k (tree/child-ids nodes heading)
                          :when (not= k id)]
                      (content-entry state (title k) k)))])))

(defn generation-context
  "回傳 {:sections [{:group :title :entries [{:label :text :source ...}]}] :chars n}；
  :source 為 :prose（帶本文）或 :intent（帶意圖）。只有段落與引言會生成本文，其他卡片回傳 nil。"
  [{:keys [nodes] :as state} id]
  (let [numbers (tree/numbering nodes)
        title #(tree/display-title nodes numbers %)
        sections (case (tree/kind nodes id)
                   :paragraph (paragraph-context state title id)
                   :intro (intro-context state title id)
                   nil)]
    (when sections
      {:sections (vec sections)
       :chars (reduce + (for [s sections
                              e (:entries s)]
                          (+ (count (:label e)) (count (:text e)))))})))
