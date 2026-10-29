(ns knot.source-ops
  "agent 的素材與事實寫入操作（SPEC 5.2、5.5、5.12）：與 knot.ops 相同的形式——
  收狀態（:nodes 為 id → 卡片、:blocks 為素材的向量、:facts 為 key → 事實）與參數，回傳
  {:state 新狀態 :label 修改組的預設名稱 :origin :manual
   :blocks [新增或修改的素材 id] :facts [新增或修改的事實 key]
   :citing [相關的卡片 id]（捨棄素材時為仍引用它的段落；修改事實時為本文用到它的段落）
   :fact-problems [{:key :error}]（修改素材後，值在新內容中找不到的事實）
   :message :note}，
  不合法時回傳 {:error \"給 agent 看的中文原因\"}，狀態一律不變。
  事實的值是否出現在素材中由 knot.sources/fact-value-error 判斷。"
  (:require [clojure.string :as str]
            [knot.ops :as ops]
            [knot.sources :as sources]
            [knot.tree :as tree]))

;; ---------------------------------------------------------------------------
;; 共用

(defn- nonblank? [s] (and (string? s) (not (str/blank? s))))

(defn- block-by-id [state id] (some #(when (= id (:block/id %)) %) (:blocks state)))

(defn- replace-block [state block]
  (update state :blocks (fn [bs] (mapv #(if (= (:block/id %) (:block/id block)) block %) bs))))

(defn- facts-of [state block-id]
  (sort-by :fact/key (filter #(= block-id (:fact/block %)) (vals (:facts state)))))

(defn- card-name [state id]
  (let [nodes (:nodes state)]
    (str "「" (tree/display-title nodes (tree/numbering nodes) id) "」")))

(defn- optional-text-error
  "選填的字串欄位：沒給為 nil（合法）；給了必須是字串。"
  [k v]
  (when (and (some? v) (not (string? v))) (str k " 必須是字串")))

(defn- clean [s] (when (nonblank? s) (str/trim s)))

(defn- block-ref [b] (str "「" (:block/title b) "」"))

(defn- missing-block-error [block-id]
  (str "找不到素材：" block-id "。請先用 search_sources 查詢有效的 block_id"))

(defn- usable-block
  "可引用的素材；找不到或已捨棄時回傳 {:error}。"
  [state block-id]
  (let [b (block-by-id state block-id)]
    (cond
      (nil? b) {:error (missing-block-error block-id)}
      (sources/discarded? b) {:error (str "素材" (block-ref b) "已被捨棄，不能使用；捨棄的素材請使用者在畫面復原")}
      :else {:block b})))

;; ---------------------------------------------------------------------------
;; 新增素材

(defn- common-error [{:keys [source title location note]}]
  (cond
    (not (nonblank? source)) "需要 source（來源檔名稱，例如「期中報告.pdf」）"
    (not (nonblank? title)) "需要 title（素材名稱，讓人一眼看出這段是什麼）"
    (optional-text-error "location" location) (optional-text-error "location" location)
    (optional-text-error "note" note) (optional-text-error "note" note)))

(defn add-excerpt
  "新增一筆節錄（:para）：source 來源檔名、title 名稱、text 原文（照抄）必填；location 位置、note 用途說明選填。
  同一來源已有相同原文（正規化後）的素材時拒絕，避免重複。"
  [state {:keys [source title text location note] :as args}]
  (let [id (random-uuid)
        dup (when (nonblank? text)
              (some #(when (and (= (:block/source %) (str/trim (str source)))
                                (not (sources/discarded? %))
                                (= (:block/kind %) :para)
                                (= (sources/normalize (:block/text %)) (sources/normalize text)))
                       %)
                    (:blocks state)))]
    (cond
      (common-error args) {:error (common-error args)}
      (not (nonblank? text)) {:error "需要 text（節錄的原文，照抄、不改字、不改數字）"}
      dup {:error (str "同一來源已有相同原文的素材" (block-ref dup) "（block_id：" (:block/id dup)
                       "），不需要重複新增；直接引用它即可")}
      :else
      {:state (update state :blocks (fnil conj [])
                      (cond-> {:block/id id :block/kind :para :block/status :confirmed
                               :block/source (str/trim source) :block/title (str/trim title) :block/text text}
                        (clean location) (assoc :block/location (clean location))
                        (clean note) (assoc :block/note (clean note))))
       :blocks [id]
       :origin :manual
       :label (str "新增節錄「" (str/trim title) "」")})))

(defn add-source-table
  "新增一張表格素材（:table）：columns 欄名（非空陣列）、rows 各列（每列長度等於欄數，
  儲存格是字串或數字，數字轉成字串）；source、title 必填，location、note 選填。表格要照原樣節錄。"
  [state {:keys [source title location note columns rows] :as args}]
  (let [id (random-uuid)]
    (cond
      (common-error args) {:error (common-error args)}
      (ops/table-content-error columns rows) {:error (ops/table-content-error columns rows)}
      :else
      {:state (update state :blocks (fnil conj [])
                      (cond-> {:block/id id :block/kind :table :block/status :confirmed
                               :block/source (str/trim source) :block/title (str/trim title)
                               :table/kind :data
                               :table/columns (vec columns)
                               :table/rows (mapv #(mapv ops/cell-text %) rows)}
                        (clean location) (assoc :block/location (clean location))
                        (clean note) (assoc :block/note (clean note))))
       :blocks [id]
       :origin :manual
       :label (str "新增表格素材「" (str/trim title) "」")})))

;; ---------------------------------------------------------------------------
;; 修改與捨棄

(defn- recheck-facts
  "素材改變後，指向它的事實逐一檢查；回傳 [{:key :error}]。"
  [state block]
  (vec (for [f (facts-of state (:block/id block))
             :let [e (sources/fact-value-error block f)]
             :when e]
         {:key (:fact/key f) :error e})))

(defn update-source-block
  "修改素材：title、text、location、note（空字串清除 location 與 note）；表格可改 columns 與 rows（要一起給）。
  已捨棄的素材不能改。改了原文或表格後重新檢查指向它的事實：仍然寫入，但把值找不到的事實列在
  :fact-problems，讓 agent 接著用 update_fact 修正——若改成拒絕，要把事實改成新值就永遠過不了
  「值必須出現在素材中」的檢查（update_fact 比對的是目前的原文）。"
  [state {:keys [id title text location note columns rows]}]
  (let [b (block-by-id state id)
        table? (some-> b sources/table?)
        content? (or (some? columns) (some? rows))]
    (cond
      (nil? b) {:error (missing-block-error id)}
      (sources/discarded? b) {:error (str "素材" (block-ref b) "已被捨棄，不能修改；捨棄的素材請使用者在畫面復原")}
      (and (nil? title) (nil? text) (nil? location) (nil? note) (not content?))
      {:error "沒有要修改的欄位：請給 title、text、location、note（表格另有 columns 與 rows）"}
      (and (some? title) (not (nonblank? title))) {:error "title 不能是空白"}
      (optional-text-error "location" location) {:error (optional-text-error "location" location)}
      (optional-text-error "note" note) {:error (optional-text-error "note" note)}
      (and (some? text) table?) {:error "表格素材沒有 text，內容請改傳 columns 與 rows"}
      (and (some? text) (not (nonblank? text))) {:error "text 不能是空白"}
      (and content? (not table?)) {:error "節錄沒有 columns 與 rows，原文請改傳 text"}
      (and content? (not (and (some? columns) (some? rows)))) {:error "columns 與 rows 要一起給（整張表取代）"}
      (and content? (ops/table-content-error columns rows)) {:error (ops/table-content-error columns rows)}
      :else
      (let [b' (cond-> b
                 (some? title) (assoc :block/title (str/trim title))
                 (some? text) (assoc :block/text text)
                 (some? location) (as-> x (if-let [l (clean location)] (assoc x :block/location l) (dissoc x :block/location)))
                 (some? note) (as-> x (if-let [n (clean note)] (assoc x :block/note n) (dissoc x :block/note)))
                 content? (assoc :table/columns (vec columns)
                                 :table/rows (mapv #(mapv ops/cell-text %) rows)))
            problems (when (or (some? text) content?) (recheck-facts state b'))]
        {:state (replace-block state b')
         :blocks [id]
         :fact-problems (vec problems)
         :origin :manual
         :label (str "修改素材" (block-ref b'))
         :message (when (seq problems)
                    (str "已修改素材，但有 " (count problems) " 個事實的值在新內容中找不到："
                         (str/join "；" (for [{:keys [key error]} problems] (str key "：" error)))
                         "。請用 update_fact 修正這些事實"))}))))

(defn- citing-nodes
  "引用素材的卡片（依文件順序）。"
  [state block-id]
  (let [nodes (:nodes state)]
    (vec (for [id (tree/document-order nodes)
               :when (some #(= block-id (:block %)) (get-in nodes [id :node/citations]))]
           id))))

(defn discard-source-block
  "捨棄素材（狀態改為 :discarded）：搜尋不到、不能再被引用；已引用它的段落保留依據並標示「已捨棄」，
  這些段落列在 :citing 提醒 agent 改寫。使用者可在畫面復原。"
  [state {:keys [id]}]
  (let [b (block-by-id state id)]
    (cond
      (nil? b) {:error (missing-block-error id)}
      (sources/discarded? b) {:error (str "素材" (block-ref b) "已經是捨棄的")}
      :else
      (let [citing (citing-nodes state id)]
        {:state (replace-block state (assoc b :block/status :discarded))
         :blocks [id]
         :citing citing
         :origin :manual
         :label (str "捨棄素材" (block-ref b))
         :message (str "已捨棄素材" (block-ref b) "；使用者可在 Knot 畫面復原"
                       (when (seq citing)
                         (str "。仍引用它的段落：" (str/join "、" (map #(card-name state %) citing))
                              "（依據會標示「已捨棄」，請確認本文是否需要改寫或改引用其他素材）")))}))))

;; ---------------------------------------------------------------------------
;; 事實

(defn- value-text [v]
  (cond (string? v) v
        (number? v) (str v)))

(defn- binding-error
  "row 與 column 的型別檢查（是否在素材內由 fact-value-error 判斷）。"
  [row column]
  (cond
    (and (some? row) (not (and (integer? row) (<= 0 row)))) "row 必須是 0 以上的整數（列索引，從 0 起算）"
    (and (some? column) (not (nonblank? column))) "column 必須是欄名字串"
    (not= (some? row) (some? column)) "row 與 column 必須同時給（指定表格的某個儲存格）"))

(defn- fact-map [{:keys [key label value unit block-id row column]}]
  (cond-> {:fact/key key :fact/label (str/trim label) :fact/value value :fact/block block-id}
    (clean unit) (assoc :fact/unit (clean unit))
    (some? row) (assoc :fact/row row :fact/column column)))

(defn add-fact
  "新增事實：key（英文小寫、數字、「.」「-」，報告內唯一）、label、value、unit（選填）、block-id（必填，不能是已捨棄的）；
  素材是表格時可給 row（0 起算）與 column（欄名）指定儲存格，兩者同時給。
  檢查 key 格式與唯一、值出現在素材中（只比對值，不含單位）。"
  [state {:keys [key label value unit block-id row column]}]
  (let [value (value-text value)
        {b :block err :error} (when block-id (usable-block state block-id))
        f (when value (fact-map {:key key :label (str label) :value value :unit unit :block-id block-id :row row :column column}))]
    (cond
      (not (nonblank? key)) {:error "需要 key（事實的識別名稱，例如 ridership.2025-11）"}
      (not (sources/fact-key-valid? key))
      {:error (str "key「" key "」格式不正確：只能用英文小寫字母、數字、「.」與「-」，且符號前後要有字母或數字（例如 saving-rate.site-a）")}
      (contains? (:facts state) key)
      {:error (str "key「" key "」已經存在（" (:fact/label (get (:facts state) key)) "＝"
                   (:fact/value (get (:facts state) key)) "）；要修改請用 update_fact")}
      (not (nonblank? label)) {:error "需要 label（事實的名稱，例如「節電率·A」）"}
      (nil? value) {:error "需要 value（值，字串或數字，必須照抄素材中的寫法）"}
      (optional-text-error "unit" unit) {:error (optional-text-error "unit" unit)}
      (nil? block-id) {:error "需要 block_id（事實所屬的素材）"}
      err {:error err}
      (binding-error row column) {:error (binding-error row column)}
      (sources/fact-value-error b f) {:error (sources/fact-value-error b f)}
      :else
      {:state (assoc-in state [:facts key] f)
       :facts [key]
       :origin :manual
       :label (str "新增事實「" (str/trim label) "」")})))

(defn- citing-fact
  "本文用到某事實的卡片（依文件順序）。"
  [state k]
  (let [uses? (fn [prose]
                (some #(and (map? %) (= "fact" (:type %)) (= k (get-in % [:attrs :key])))
                      (tree-seq coll? seq prose)))
        nodes (:nodes state)]
    (vec (for [id (tree/document-order nodes) :when (uses? (get-in nodes [id :node/prose]))] id))))

(defn update-fact
  "修改事實：label、value、unit（空字串清除）、block-id、row 與 column（要同時給；都給 nil 表示改為不指定儲存格）。
  key 不能改。檢查同 add-fact。本文中的 {{fact:key}} 之後顯示新值，不必改本文；用到這個事實的段落列在 :citing。"
  [state {:keys [key label value unit block-id] :as args}]
  (let [f (get (:facts state) key)
        value' (when (some? value) (value-text value))
        bind? (or (contains? args :row) (contains? args :column))
        {:keys [row column]} args
        new-block-id (or block-id (:fact/block f))
        {b :block err :error} (when f (usable-block state new-block-id))
        f' (when f
             (cond-> f
               (some? label) (assoc :fact/label (str/trim (str label)))
               (some? value') (assoc :fact/value value')
               (some? unit) (as-> x (if-let [u (clean unit)] (assoc x :fact/unit u) (dissoc x :fact/unit)))
               (some? block-id) (assoc :fact/block block-id)
               bind? (as-> x (if (some? row)
                               (assoc x :fact/row row :fact/column column)
                               (dissoc x :fact/row :fact/column)))))]
    (cond
      (nil? f) {:error (str "找不到事實：" key "。請用 list_facts 查詢有效的 key")}
      (and (nil? label) (nil? value) (nil? unit) (nil? block-id) (not bind?))
      {:error "沒有要修改的欄位：請給 label、value、unit、block_id、row 與 column"}
      (and (some? label) (not (nonblank? label))) {:error "label 不能是空白"}
      (and (some? value) (nil? value')) {:error "value 必須是字串或數字"}
      (and (some? value') (str/blank? value')) {:error "value 不能是空白"}
      (optional-text-error "unit" unit) {:error (optional-text-error "unit" unit)}
      err {:error err}
      (and bind? (binding-error row column)) {:error (binding-error row column)}
      (sources/fact-value-error b f') {:error (sources/fact-value-error b f')}
      :else
      (let [citing (citing-fact state key)]
        {:state (assoc-in state [:facts key] f')
         :facts [key]
         :citing citing
         :origin :manual
         :label (str "修改事實「" (:fact/label f') "」")
         :message (str "已修改事實「" (:fact/label f') "」；本文中的 {{fact:" key "}} 會顯示新值，不必改本文"
                       (when (seq citing)
                         (str "。用到它的段落：" (str/join "、" (map #(card-name state %) citing)))))}))))

;; 刪除事實：本文還用到時拒絕，避免留下找不到的事實引用
(defn delete-fact
  "刪除事實（例如登記錯的事實）。本文還用到 {{fact:key}} 的段落時拒絕並列出，請先改寫這些段落。"
  [state {:keys [key]}]
  (let [f (get (:facts state) key)
        citing (when f (citing-fact state key))]
    (cond
      (nil? f) {:error (str "找不到事實：" key "。請用 list_facts 查詢有效的 key")}
      (seq citing)
      {:error (str "事實「" (:fact/label f) "」還被這些段落的本文使用：" (str/join "、" (map #(card-name state %) citing))
                   "。請先用 write_prose 改寫這些段落，不再使用 {{fact:" key "}}，再刪除")}
      :else
      {:state (update state :facts dissoc key)
       :origin :manual
       :label (str "刪除事實「" (:fact/label f) "」")
       :message (str "已刪除事實「" (:fact/label f) "」（" key "）；使用者可在 Knot 畫面復原")})))

;; ---------------------------------------------------------------------------
;; 批次

(def max-batch-facts 100)
(def max-batch-excerpts 50)

(defn- add-batch
  "批次新增的共用部分：items 逐筆套 add-one（收 state 與一筆參數，回傳單筆結果），
  累積狀態讓同批的檢查看得到前面新增的（重複 key、重複原文）。
  任何一筆錯誤就整批不寫，錯誤列出每一筆（describe 產生「items[i]（說明）」）。
  成功回傳 {:state :origin :label} 加上各筆結果的 result-key 向量。"
  [state items {:keys [field limit add-one describe result-key label]}]
  (cond
    (not (sequential? items)) {:error (str "需要 " field "（陣列，每項是一筆）")}
    (empty? items) {:error (str field " 不能是空陣列")}
    (> (count items) limit) {:error (str field " 最多 " limit " 筆，這次給了 " (count items) " 筆；請分批新增")}
    :else
    (let [step (fn [acc [i item]]
                 (let [r (if (map? item) (add-one (:state acc) item) {:error (str field " 的每一項必須是物件")})]
                   (if-let [e (:error r)]
                     (update acc :errors conj (str field "[" i "]" (describe item) "：" e))
                     (-> acc (assoc :state (:state r)) (update :results into (get r result-key))))))
          {:keys [state results errors]} (reduce step {:state state :results [] :errors []} (map-indexed vector items))]
      (if (seq errors)
        {:error (str "整批沒有寫入，有 " (count errors) " 筆有問題：" (str/join "；" errors) "。請修正後整批重送")}
        {:state state result-key results :origin :manual :label (label (count items))}))))

(defn add-facts
  "批次新增事實：facts 是陣列（最多 100 筆），每項欄位同 add-fact（key、label、value、unit、block-id、row、column）。
  全部合法才寫入；同一批內 key 重複也是錯誤。"
  [state {:keys [facts]}]
  (add-batch state facts
             {:field "facts" :limit max-batch-facts :result-key :facts
              :add-one add-fact
              :describe #(if (nonblank? (:key %)) (str "（" (:key %) "）") "")
              :label #(str "新增 " % " 筆事實")}))

(defn add-excerpts
  "批次新增節錄：excerpts 是陣列（最多 50 筆），每項欄位同 add-excerpt（source、title、text、location、note）。
  全部合法才寫入；同一批內原文重複（同一來源）也是錯誤。"
  [state {:keys [excerpts]}]
  (add-batch state excerpts
             {:field "excerpts" :limit max-batch-excerpts :result-key :blocks
              :add-one add-excerpt
              :describe #(if (nonblank? (:title %)) (str "（" (str/trim (:title %)) "）") "")
              :label #(str "新增 " % " 筆節錄")}))

;; ---------------------------------------------------------------------------
;; 依據

(defn- citation-card-error [state id]
  (let [n (get (:nodes state) id)]
    (cond
      (nil? n) (str "找不到卡片：" id)
      (not (#{:paragraph :intro} (:node/kind n))) "依據只能加在段落與引言上")))

(defn add-citation
  "把素材加入段落或引言的依據；where 選填，說明引用的範圍（例如「欄：場域、節電率」）。
  已捨棄的素材不能加；已經是依據時不寫入。"
  [state {:keys [id block-id where]}]
  (let [{b :block err :error} (usable-block state block-id)
        cites (get-in state [:nodes id :node/citations])]
    (cond
      (citation-card-error state id) {:error (citation-card-error state id)}
      err {:error err}
      (optional-text-error "where" where) {:error (optional-text-error "where" where)}
      (some #(= block-id (:block %)) cites)
      {:state state :touched [id] :origin :manual
       :note (str "素材" (block-ref b) "已經是" (card-name state id) "的依據，沒有變動")}
      :else
      {:state (update-in state [:nodes id :node/citations] (fnil conj [])
                         (cond-> {:block block-id} (clean where) (assoc :where (clean where))))
       :touched [id]
       :origin :manual
       :label (str "加入依據：" (card-name state id) "←" (block-ref b))})))

(defn remove-citation
  "從段落或引言的依據移除素材（含已捨棄的素材）。不是依據時回錯誤。"
  [state {:keys [id block-id]}]
  (let [cites (get-in state [:nodes id :node/citations])
        b (block-by-id state block-id)]
    (cond
      (citation-card-error state id) {:error (citation-card-error state id)}
      (not-any? #(= block-id (:block %)) cites)
      {:error (str "素材" (if b (block-ref b) block-id) "不是" (card-name state id) "的依據；現有的依據請用 get_card 查看")}
      :else
      {:state (let [kept (vec (remove #(= block-id (:block %)) cites))]
                (if (seq kept)
                  (assoc-in state [:nodes id :node/citations] kept)
                  (update-in state [:nodes id] dissoc :node/citations)))
       :touched [id]
       :origin :manual
       :label (str "移除依據：" (card-name state id) "←" (if b (block-ref b) block-id))})))
