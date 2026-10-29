(ns knot.context
  "生成某張卡片的本文時，AI 會讀到的內容（SPEC 5.4）。
  周邊卡片（上層、鄰近、關係）只帶意圖，並以 :has-prose 標明對方是否已有本文，
  agent 需要銜接或用到某段的結果時再用 get_card 取本文；例外：引言，下層內容有本文時帶本文，
  下層是標題時帶該標題引言的本文，否則帶底下各段落的本文（合計超過 intro-prose-limit 字改帶意圖並提示 get_cards）；
  引用的表格帶欄位與全部列（儲存格合計超過 table-cell-limit 字只帶欄位與列數）；
  本文中的交互參照顯示為「表「名稱」（tab:x）」；
  段落的前一段與依據的目標有本文時帶本文（:source :prose），合計超過 prose-limit 字時全部改帶意圖，
  並以 :prose-too-long 標明、請用 get_card 取本文。
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

;; 撰寫參考內的本文：交互參照顯示為「表「名稱」（tab:x）」，找不到對應卡片時維持 {label}
(defn- ref-display-fn [nodes]
  (let [by-label (into {} (keep (fn [n] (when (:node/label n) [(:node/label n) n]))) (vals nodes))]
    (fn [label]
      (when-let [n (by-label label)]
        (when-let [k (case (:node/kind n) :table "表" :figure "圖" nil)]
          (str k "「" (:node/title n) "」（" label "）"))))))

(defn- ctx-text
  "撰寫參考用的本文純文字（交互參照帶類型與名稱）；doc 為 nil 時回傳 nil。"
  [{:keys [nodes facts]} doc]
  (some->> doc (#(prose/doc->text facts % (ref-display-fn nodes)))))

(defn- entry [label text & {:as more}]
  (merge {:label label :text (or text "")} more))

(defn- section [group title entries]
  (when (seq entries)
    {:group group :title title :entries (vec entries)}))

(defn- intent-entry [nodes label id & more]
  (apply entry label (get-in nodes [id :node/draft]) :source :intent more))

(defn- has-prose? [{:keys [nodes] :as state} id]
  (not (str/blank? (ctx-text state (get-in nodes [id :node/prose])))))

(defn- ref-entry
  "周邊卡片：只帶意圖；對方已有本文時加 :has-prose true（agent 需要時用 get_card 取本文）。"
  [state label id & more]
  (apply intent-entry (:nodes state) label id
         (concat more (when (has-prose? state id) [:has-prose true]))))

(defn- content-entry
  "有本文時帶本文，否則帶意圖。"
  [{:keys [nodes] :as state} label id & more]
  (let [text (ctx-text state (get-in nodes [id :node/prose]))]
    (if (str/blank? text)
      (apply intent-entry nodes label id more)
      (apply entry label text :source :prose more))))

;; 前一段與依據目標的本文合計上限（字數，同 knot.prose/char-count）
(def prose-limit 2000)

(defn- prose-entries
  "前一段與依據目標：有本文時帶本文，否則帶意圖（沿用 ref-entry）。
  ids 是 [[id label & more] ...]；有本文者的字數合計超過 prose-limit 時，全部帶意圖，
  有本文的加 :has-prose 與 :prose-too-long。回傳與 ids 同序的 entry 向量。"
  [{:keys [nodes facts] :as state} items]
  (let [total (reduce + (for [[id] items] (prose/char-count facts (get-in nodes [id :node/prose]))))
        too-long? (> total prose-limit)]
    (vec (for [[id label & more] items]
           (if too-long?
             (let [e (apply ref-entry state label id more)]
               (cond-> e (:has-prose e) (assoc :prose-too-long true)))
             (apply content-entry state label id more))))))

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

;; 引用的表格儲存格文字合計不超過這個字數時，帶欄位與全部列
(def table-cell-limit 3000)

(defn- table-block-text [{:table/keys [columns rows]} where]
  (let [cells (concat columns (apply concat rows))
        total (reduce + (map #(count (str %)) cells))]
    (if (<= total table-cell-limit)
      (str (table-content-text {:columns columns :rows rows})
           (when where (str "\n（" where "）")))
      (str "欄位：" (str/join "、" columns)
           "；" (count rows) " 列"
           (when where (str "（" where "）"))
           "；列太多未帶入，請以 get_source_block 取得"))))

(defn- citations [{:keys [nodes blocks]} id]
  (let [by-id (into {} (map (juxt :block/id identity)) blocks)]
    (for [{:keys [block where]} (get-in nodes [id :node/citations])
          :let [b (by-id block)]
          :when b]
      (entry (str (if (= :table (:block/kind b)) "表 " "段 ") (:block/title b))
             (if (= :table (:block/kind b))
               (table-block-text b where)
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
        basis-targets (for [r outgoing :when (= :basis (:relation/type r))] (:relation/to r))
        ;; 前一段與依據目標一起計算字數上限
        near (prose-entries state
                            (concat (when prev [[prev (title prev) :relation (relation-text (tree/relation-between relations prev id))]])
                                    (for [to basis-targets] [to (title to)])))
        prev-entry (when prev (first near))
        basis-entries (if prev (rest near) near)
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
                     (when prev [prev-entry]))
            (section :neighbors "下一段"
                     (when nxt
                       [(ref-entry state (title nxt) nxt
                                       :relation (relation-text (tree/relation-between relations id nxt)))]))]
           (for [t tree/remote-relation-types]
             (section :relations (remote-section-title t)
                      (if (= t :basis)
                        basis-entries
                        (for [r outgoing :when (= t (:relation/type r))
                              :let [to (:relation/to r)]]
                          (ref-entry state (title to) to)))))
           ;; 摘要本段的段落依賴本段，只帶意圖讓 AI 知道本段的結果會被誰彙整
           [(section :relations "摘要本段的段落"
                     (for [s summarizers] (ref-entry state (title s) s)))]))))

;; 引言的下層內容中，標題底下段落的本文合計上限（字數，同 knot.prose/char-count）
(def intro-prose-limit 6000)

(defn- intro-context [{:keys [nodes facts] :as state} title id]
  (let [heading (tree/parent nodes id)
        kids (remove #(= % id) (tree/child-ids nodes heading))
        ;; 下層標題 → 它底下所有段落（後代，依文件順序）
        under (fn [h] (filterv #(tree/paragraph? nodes %) (tree/descendant-ids nodes h)))
        owner (fn [p] (some #(when (tree/heading? nodes %) %) (tree/ancestor-ids nodes p)))
        ;; 有引言本文的標題只帶引言，不展開底下的段落
        intro-text (fn [h] (when-let [i (tree/intro-id nodes h)]
                             (let [t (ctx-text state (get-in nodes [i :node/prose]))]
                               (when-not (str/blank? t) t))))
        expanded (->> kids
                      (filter #(and (tree/heading? nodes %) (not (intro-text %))))
                      (mapcat under))
        total (reduce + (for [p expanded] (prose/char-count facts (get-in nodes [p :node/prose]))))
        too-long? (> total intro-prose-limit)
        para-entry (fn [p]
                     (let [label (str (title (owner p)) "／" (title p))]
                       (if too-long?
                         (let [e (ref-entry state label p)]
                           (cond-> e (:has-prose e) (assoc :prose-too-long true)))
                         (content-entry state label p))))
        entries (vec (mapcat
                      (fn [k]
                        (cond
                          (tree/paragraph? nodes k) [(content-entry state (title k) k)]
                          (tree/heading? nodes k)
                          (if-let [t (intro-text k)]
                            [(entry (str (title k) " 引言") t :source :prose)]
                            (map para-entry (under k)))
                          :else nil))
                      kids))
        entries (cond-> entries
                  too-long?
                  (conj (entry "提示"
                               (str "標題底下段落的本文合計超過 " intro-prose-limit
                                    " 字，改帶意圖；需要本文時請以 get_cards 取得")
                               :source :note)))]
    (keep identity
          [(style-section state)
           (section :upper "章節路徑" (heading-path nodes title id))
           (section :self "引言的意圖" (cons (intent-entry nodes "引言" id)
                                              (target-entry nodes id)))
           (section :children "這一層的下層內容" entries)])))

(defn generation-context
  "回傳 {:sections [{:group :title :entries [{:label :text :source ...}]}] :chars n}；
  :source 為 :prose（帶本文）、:intent（帶意圖）或 :target（目標字數）；:has-prose true 表示帶的是意圖、但對方已有本文；
  :prose-too-long true 表示前一段與依據目標（或引言下層標題底下的段落）的本文合計超過上限，改帶意圖；:note 為提示。只有段落與引言會生成本文，其他卡片回傳 nil。"
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
