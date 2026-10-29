(ns knot.context
  "生成某張卡片的本文時，AI 會讀到的內容（SPEC 5.4）。
  周邊卡片（上層、鄰近、關係）一律只帶意圖，並以 :has-prose 標明對方是否已有本文，
  agent 需要銜接或用到某段的結果時再用 get_card 取本文；例外是引言，下層內容有本文時帶本文。
  編輯區的預覽與 MCP 的 get_writing_context 共用這份結果。"
  (:require [clojure.string :as str]
            [knot.prose :as prose]
            [knot.sources :as sources]
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

(def group-label
  "編輯區摘要列的分組，依顯示順序。"
  [[:upper "上層"] [:self "本段"] [:neighbors "鄰近"] [:children "下層"] [:relations "關係"] [:sources "素材"] [:style "風格"]])

(defn prose-text
  "本文（ProseMirror JSON 的 EDN 形式）轉成純文字；事實引用換成值與單位，交互參照換成 label，
  清單項目前加「- 」或「1. 」（實作在 knot.prose/doc->text）。"
  [facts doc]
  (prose/doc->text facts doc))

(defn table-content-text
  "表格卡片自己的內容轉成文字：第一行是欄名，之後每列一行，儲存格以「｜」分隔；沒有內容時為 nil。"
  [{:keys [columns rows]}]
  (when (seq columns)
    (str/join "\n" (map #(str/join "｜" %) (cons columns rows)))))

(defn- entry [label text & {:as more}]
  (merge {:label label :text (or text "")} more))

(defn- section [group title entries]
  (when (seq entries)
    {:group group :title title :entries (vec entries)}))

(defn- intent-entry [nodes label id & more]
  (apply entry label (get-in nodes [id :node/draft]) :source :intent more))

(defn- has-prose? [{:keys [nodes facts]} id]
  (not (str/blank? (some->> (get-in nodes [id :node/prose]) (prose-text facts)))))

(defn- ref-entry
  "周邊卡片：只帶意圖；對方已有本文時加 :has-prose true（agent 需要時用 get_card 取本文）。"
  [state label id & more]
  (apply intent-entry (:nodes state) label id
         (concat more (when (has-prose? state id) [:has-prose true]))))

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
    ;; 標題的目標字數（整章／整節）一併帶上，讓 agent 知道本段在篇幅中的位置
    (apply intent-entry nodes (title a) a
           (when-let [t (get-in nodes [a :node/target-chars])] [:target-chars t]))))

(defn- target-entry
  "本卡片的目標字數：有設定時為一個 entry（text 為「約 N 字」），否則為空。"
  [nodes id]
  (when-let [t (get-in nodes [id :node/target-chars])]
    [(entry "目標字數" (str "約 " t " 字（撰寫時控制篇幅）") :source :target :target-chars t)]))

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
             :warn (= :discarded (:block/status b))
             :discarded (= :discarded (:block/status b))))))

(defn- cited-facts
  "只帶引用素材上的事實，控制提示詞長度。"
  [{:keys [nodes facts]} id]
  (let [cited (cited-block-ids nodes id)]
    (for [{:fact/keys [key label block] :as f} (sort-by :fact/key (vals facts))
          :when (cited block)]
      (entry label (sources/fact-display f) :key key))))

(defn- relation-text [rel]
  (cond
    (nil? rel) "未標註關係"
    (:relation/pending rel) (str (some-> (:relation/type rel) relation-label (str "，")) "待確認")
    :else (relation-label (:relation/type rel))))

(defn- format-at [at]
  #?(:clj (when at (.format (java.text.SimpleDateFormat. "M/d HH:mm") at))
     :cljs (when at (.toLocaleString at "zh-TW" #js {:month "numeric" :day "numeric" :hour "2-digit" :minute "2-digit"}))))

(defn style-hint
  "風格在撰寫參考中的一行提示（給 agent）：字數與最後修改時間，請依 get_style 讀取全文；沒有風格時說明尚未設定。"
  [state]
  (let [text (str/trim (or (get-in state [:style :style/text]) ""))]
    (if (str/blank? text)
      "尚未設定風格"
      (let [{:keys [updated-at updated-by]} (:style-info state)]
        (str "共 " (count text) " 字；請依 get_style 讀取全文"
             (when updated-at (str "（最後修改：" (some-> updated-by name) " " (format-at updated-at) "）")))))))

(defn style-section
  "「風格」一組：一個 entry，text 為提示，:summary 為風格開頭約 60 字（畫面顯示用），:chars 為字數。"
  [state]
  (let [text (str/trim (or (get-in state [:style :style/text]) ""))
        summary (let [one-line (str/replace text #"\s+" " ")]
                  (if (> (count one-line) 60) (str (subs one-line 0 60) "…") one-line))]
    (section :style "風格"
             [(entry "風格" (style-hint state) :source :style :chars (count text)
                     :summary (when-not (str/blank? text) summary))])))

(defn- paragraph-context [{:keys [nodes relations] :as state} title id]
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
           [(style-section state)
            (section :upper "章節路徑" (heading-path nodes title id))
            (section :upper "所屬標題的引言"
                     (when intro
                       (let [e (ref-entry state "引言" intro)]
                         (when (or (not (str/blank? (:text e))) (:has-prose e)) [e]))))
            (section :self "本段" (cons (intent-entry nodes (:node/title node) id)
                                       (target-entry nodes id)))
            (section :self "下層表格與圖"
                     (for [k (tree/child-ids nodes id)
                           :let [label (str (get-in nodes [k :node/label]) " " (title k))
                                 content (table-content-text (get-in nodes [k :node/table-content]))]]
                       ;; 表格有自己的內容時，意圖之後接欄名與各列
                       (if content
                         (entry label (str/join "\n" (remove str/blank? [(get-in nodes [k :node/draft]) content]))
                                :source :prose)
                         (intent-entry nodes label k))))
            (section :sources "依據" (citations state id))
            (section :sources "可用事實" (cited-facts state id))
            (section :neighbors "前一段"
                     (when prev
                       [(ref-entry state (title prev) prev
                                       :relation (relation-text (tree/relation-between relations prev id)))]))
            (section :neighbors "下一段"
                     (when nxt
                       [(ref-entry state (title nxt) nxt
                                       :relation (relation-text (tree/relation-between relations id nxt)))]))]
           (for [t tree/remote-relation-types]
             (section :relations (remote-section-title t)
                      (for [r outgoing :when (= t (:relation/type r))
                            :let [to (:relation/to r)]]
                        (ref-entry state (title to) to))))
           ;; 摘要本段的段落依賴本段，只帶意圖讓 AI 知道本段的結果會被誰彙整
           [(section :relations "摘要本段的段落"
                     (for [s summarizers] (ref-entry state (title s) s)))]))))

(defn- intro-context [{:keys [nodes] :as state} title id]
  (let [heading (tree/parent nodes id)]
    (keep identity
          [(style-section state)
           (section :upper "章節路徑" (heading-path nodes title id))
           (section :self "引言的意圖" (cons (intent-entry nodes "引言" id)
                                              (target-entry nodes id)))
           (section :children "這一層的下層內容"
                    (for [k (tree/child-ids nodes heading)
                          :when (not= k id)]
                      (content-entry state (title k) k)))])))

(defn generation-context
  "回傳 {:sections [{:group :title :entries [{:label :text :source ...}]}] :chars n}；
  :source 為 :prose（帶本文）、:intent（帶意圖）或 :target（目標字數）；:has-prose true 表示帶的是意圖、但對方已有本文。只有段落與引言會生成本文，其他卡片回傳 nil。"
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
