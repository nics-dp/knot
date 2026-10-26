(ns knot.context
  "生成某張卡片的本文時，AI 會讀到的內容（SPEC 5.4）。
  以相關卡片的意圖取代全文，讓生成貼近使用者想法並減少 context 佔用。
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

(defn- entry [label text & {:as more}]
  (merge {:label label :text (or text "")} more))

(defn- section [title entries]
  (when (seq entries)
    {:title title :entries (vec entries)}))

(defn- heading-path [nodes title id]
  (for [a (reverse (tree/ancestor-ids nodes id))
        :when (tree/heading? nodes a)]
    (entry (title a) (get-in nodes [a :node/draft]))))

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

(defn- relation-text [rel]
  (cond
    (nil? rel) "未標註關係"
    (:relation/pending rel) (str (some-> (:relation/type rel) relation-label (str "，")) "待確認")
    :else (relation-label (:relation/type rel))))

(defn- paragraph-context [{:keys [nodes relations facts report] :as state} title id]
  (let [node (get nodes id)
        prev (tree/prev-paragraph nodes id)
        nxt (let [n (tree/next-sibling nodes id)] (when (tree/paragraph? nodes n) n))
        {:keys [outgoing incoming]} (tree/remote-relations relations id)
        summarizers (for [r incoming :when (= :summarizes (:relation/type r))]
                      (:relation/from r))]
    (keep identity
          (concat
           [(section "文風" [(entry "文風設定" (:report/style-name report))])
            (section "章節路徑" (heading-path nodes title id))
            (section "本段" [(entry (:node/title node) (:node/draft node))])
            (section "依據" (citations state id))
            (section "可用事實" (for [{:fact/keys [key label value unit]} (vals facts)]
                              (entry label (str value unit) :key key)))
            (section "前一段"
                     (when prev
                       [(entry (title prev) (get-in nodes [prev :node/draft])
                               :relation (relation-text (tree/relation-between relations prev id)))]))
            (section "下一段"
                     (when nxt
                       [(entry (title nxt) (get-in nodes [nxt :node/draft])
                               :relation (relation-text (tree/relation-between relations id nxt)))]))
            (section "摘要本段的段落"
                     (for [s summarizers]
                       (entry (title s) (get-in nodes [s :node/draft]))))]
           ;; 本段摘要、依據、呼應、對比的段落：一樣只帶意圖
           (for [t tree/remote-relation-types]
             (section (remote-section-title t)
                      (for [r outgoing :when (= t (:relation/type r))
                            :let [to (:relation/to r)]]
                        (entry (title to) (get-in nodes [to :node/draft])))))
           [(section "下層表格與圖"
                     (for [k (tree/child-ids nodes id)]
                       (entry (str (get-in nodes [k :node/label]) " " (title k))
                              (get-in nodes [k :node/draft]))))]))))

(defn- intro-context [{:keys [nodes report]} title id]
  (let [heading (tree/parent nodes id)]
    (keep identity
          [(section "文風" [(entry "文風設定" (:report/style-name report))])
           (section "章節路徑" (heading-path nodes title id))
           (section "引言的意圖" [(entry "引言" (get-in nodes [id :node/draft]))])
           (section "這一層的下層內容"
                    (for [k (tree/child-ids nodes heading)
                          :when (not= k id)]
                      (entry (title k) (get-in nodes [k :node/draft]))))])))

(defn generation-context
  "回傳 {:sections [{:title :entries [{:label :text ...}]}] :chars n}；
  只有段落與引言會生成本文，其他卡片回傳 nil。"
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
