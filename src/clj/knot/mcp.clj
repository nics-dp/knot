(ns knot.mcp
  "MCP 端點（SPEC 5.12）：Streamable HTTP 傳輸、JSON-RPC 2.0，POST /mcp。
  工具以資料登記：{:name :description :inputSchema :handler}，handler 收 (conn args)，
  回傳結果 map（key 為英文 snake_case 的 keyword）；工具層面的錯誤丟 (tool-error! 中文說明)。
  keyword 與 JSON 字串的轉換集中在 `write-json` 與 `read-json`。"
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [jsonista.core :as json]
            [knot.context :as context]
            [knot.db :as db]
            [knot.notes :as notes]
            [knot.ops :as ops]
            [knot.prose :as prose]
            [knot.source-ops :as source-ops]
            [knot.sources :as sources]
            [knot.sync :as sync]
            [knot.tree :as tree]
            [knot.ui :as ui]))

(def ^:private mapper
  (json/object-mapper {:encode-key-fn name :decode-key-fn false}))

(defn- write-json [x] (json/write-value-as-string x mapper))
(defn- read-json [s] (json/read-value s mapper))

;; ---------------------------------------------------------------------------
;; 工具共用

(defn- tool-error!
  "工具層面的錯誤：以 isError 回給 agent，不是 JSON-RPC error。"
  [msg]
  (throw (ex-info msg {::tool-error true})))

(defn- current-state
  "目前報告的狀態，供 knot.tree 與 knot.context 使用。"
  [conn]
  (ui/from-server (db/load-state (d/db conn))))

(defn- card-id
  "取出參數 card_id 並確認卡片存在。"
  [{:keys [nodes]} args]
  (let [raw (get args "card_id")
        id (when (string? raw) (parse-uuid raw))]
    (cond
      (nil? raw) (tool-error! "缺少參數 card_id")
      (nil? id) (tool-error! (str "card_id 格式不正確（應為 UUID）：" raw))
      (not (contains? nodes id)) (tool-error! (str "找不到卡片：" raw "。請先用 get_outline 取得有效的 card_id"))
      :else id)))

(defn- status-name [node] (name (or (:node/status node) :none)))

(defn- card-title
  "純名稱，不含編號（標題的名稱、段落／表／圖的標題，引言為「引言」）；編號另放在 number 欄位。"
  [nodes _numbers id]
  (let [n (get nodes id)]
    (case (:node/kind n)
      :heading (:node/name n)
      :intro "引言"
      (:node/title n))))

(defn- inst-str [d] (some-> d .toInstant str))

(defn- truncate [s n]
  (let [s (or s "")]
    (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- has-prose? [node]
  (boolean (some-> (:node/prose node) :content seq)))

;; ---------------------------------------------------------------------------
;; get_outline

(defn- char-fields
  "chars（段落、引言、標題的字數；標題為整章／整節合計，表與圖沒有）與 target_chars（有設定才有）。"
  [counts n]
  (cond-> {}
    (contains? counts (:node/id n)) (assoc :chars (get counts (:node/id n)))
    (:node/target-chars n) (assoc :target_chars (:node/target-chars n))))

(defn- get-outline [conn args]
  (let [full? (let [v (get args "full_intent")]
                (when-not (or (nil? v) (boolean? v)) (tool-error! "full_intent 必須是 true 或 false"))
                (true? v))
        {:keys [nodes report] :as state} (current-state conn)
        counts (tree/char-counts state)
        numbers (tree/numbering nodes)
        todo-counts (notes/todo-counts (d/db conn))]
    {:report {:title (:report/title report) :subtitle (:report/subtitle report)}
     :cards (vec (for [id (tree/document-order nodes)
                       :let [n (get nodes id)]]
                   (cond-> {:card_id (str id)
                            :kind (name (:node/kind n))
                            :depth (tree/depth nodes id)
                            :parent_id (some-> (:node/parent n) str)
                            :number (ops/heading-number nodes numbers id)
                            :title (card-title nodes numbers id)
                            :intent (if full? (or (:node/draft n) "") (truncate (:node/draft n) 80))
                            :status (status-name n)
                            :has_prose (has-prose? n)
                            :version (:node/version n)}
                     true (merge (char-fields counts n))
                     (:node/label n) (assoc :label (:node/label n))
                     (pos? (get todo-counts id 0)) (assoc :open_todos (get todo-counts id)))))}))

;; ---------------------------------------------------------------------------
;; get_card

(defn- relation-type-label [r]
  (some-> (:relation/type r) context/relation-label))

(defn- neighbor [nodes numbers id rel]
  (when id
    {:card_id (str id)
     :number (ops/heading-number nodes numbers id)
     :title (card-title nodes numbers id)
     :type (relation-type-label rel)
     :pending (boolean (:relation/pending rel))}))

(defn- remote-entries [nodes numbers rels other-key]
  (vec (for [r rels
             :let [other (get r other-key)]]
         {:type (relation-type-label r)
          :card_id (str other)
          :number (ops/heading-number nodes numbers other)
          :title (card-title nodes numbers other)
          :position (tree/position-label nodes numbers other)})))

(defn- history-entry
  "get_card 的 history 項目；名稱、意圖有變時附 text_changes（不含本文全文）。"
  [{:keys [at by label kind fields undone? text-changes]}]
  (cond-> {:at (inst-str at)
           :by (name by)
           :label label
           :kind (name kind)
           :fields (vec (sort (map name fields)))
           :undone (boolean undone?)}
    (seq text-changes) (assoc :text_changes
                              (into {} (map (fn [[k v]] [(name k) {:before (:before v) :after (:after v)}]))
                                    text-changes))))

(defn- get-card [conn args]
  (let [{:keys [nodes relations blocks] :as state} (current-state conn)
        id (card-id state args)
        n (get nodes id)
        numbers (tree/numbering nodes)
        paragraph? (= :paragraph (:node/kind n))
        prev (when paragraph? (tree/prev-paragraph nodes id))
        nxt (when paragraph?
              (let [x (tree/next-sibling nodes id)] (when (tree/paragraph? nodes x) x)))
        {:keys [outgoing incoming]} (tree/remote-relations relations id)
        block-by-id (into {} (map (juxt :block/id identity)) blocks)]
    (cond->
     {:card_id (str id)
      :kind (name (:node/kind n))
      :number (ops/heading-number nodes numbers id)
      :title (card-title nodes numbers id)
      :path (vec (for [a (reverse (tree/ancestor-ids nodes id))
                       :when (tree/heading? nodes a)]
                   {:number (ops/heading-number nodes numbers a)
                    :title (card-title nodes numbers a)}))
      :parent_id (some-> (:node/parent n) str)
      :intent (or (:node/draft n) "")
      :status (status-name n)
      :version (:node/version n)
      :prose (when (has-prose? n) (prose/doc->markup (:node/prose n)))
      :unbound_numbers (if (has-prose? n) (prose/unbound-numbers (:facts state) (:node/prose n)) [])
      :citations (vec (for [{:keys [block where]} (:node/citations n)
                            :let [b (block-by-id block)]
                            :when b]
                        (cond-> {:block_id (str block) :block_title (:block/title b) :where where}
                          (sources/discarded? b) (assoc :discarded true))))
      :relations {:prev (neighbor nodes numbers prev (when prev (tree/relation-between relations prev id)))
                  :next (neighbor nodes numbers nxt (when nxt (tree/relation-between relations id nxt)))
                  :outgoing (remote-entries nodes numbers outgoing :relation/to)
                  :incoming (remote-entries nodes numbers incoming :relation/from)}
      :children (vec (for [k (tree/child-ids nodes id)]
                       (cond-> {:card_id (str k)
                                :kind (name (tree/kind nodes k))
                                :number (ops/heading-number nodes numbers k)
                                :title (card-title nodes numbers k)}
                         (get-in nodes [k :node/label]) (assoc :label (get-in nodes [k :node/label])))))
      :history (mapv history-entry (take-last 10 (db/node-history (d/db conn) id)))
      :notes (vec (for [note (notes/list-notes (d/db conn))
                        :when (= id (:note/node note))]
                    (cond-> {:note_id (str (:note/id note))
                             :author (name (:note/author note))
                             :text (:note/text note)
                             :at (inst-str (:note/at note))}
                      (:note/todo note) (assoc :todo true :state (name (:note/state note)))
                      (:note/reply-to note) (assoc :reply_to (str (:note/reply-to note))))))}
      true (merge (char-fields (tree/char-counts state) n))
      (:node/label n) (assoc :label (:node/label n))
      (seq (:columns (:node/table-content n))) (assoc :table (select-keys (:node/table-content n) [:columns :rows])))))

;; ---------------------------------------------------------------------------
;; get_writing_context

(def ^:private max-batch 30)

(defn- get-cards
  "一次取得多張卡片，每張與 get_card 相同；找不到或格式不對的 id 列在 errors，不影響其他卡片。"
  [conn args]
  (let [ids (get args "card_ids")]
    (when-not (and (sequential? ids) (seq ids) (every? string? ids))
      (tool-error! "card_ids 必須是非空的字串陣列（卡片 id，由 get_outline 取得）"))
    (when (> (count ids) max-batch)
      (tool-error! (str "一次最多 " max-batch " 張卡片，收到 " (count ids) " 張；請分批呼叫")))
    (let [results (for [raw (distinct ids)]
                    (try {:card (get-card conn {"card_id" raw})}
                         (catch clojure.lang.ExceptionInfo e
                           (if (::tool-error (ex-data e))
                             {:error {:card_id raw :error (ex-message e)}}
                             (throw e)))))]
      (cond-> {:cards (vec (keep :card results))}
        (some :error results) (assoc :errors (vec (keep :error results)))))))

(defn- get-writing-context [conn args]
  (let [state (current-state conn)
        id (card-id state args)
        ctx (context/generation-context state id)]
    (when-not ctx (tool-error! "只有段落與引言會撰寫本文"))
    {:card_id (str id)
     :sections (vec (for [{:keys [group title entries]} (:sections ctx)]
                      {:group (name group)
                       :title title
                       :entries (vec (for [e entries]
                                       (cond-> {:label (:label e)
                                                :text (:text e)
                                                :source (some-> (:source e) name)}
                                         (:relation e) (assoc :relation (:relation e))
                                         (:has-prose e) (assoc :has_prose true)
                                         (:warn e) (assoc :warn true)
                                         (:discarded e) (assoc :discarded true)
                                         (:target-chars e) (assoc :target_chars (:target-chars e))
                                         (:key e) (assoc :key (:key e)))))}))
     :context_chars (:chars ctx)
     :note "周邊卡片只帶意圖（has_prose 為 true 表示對方已有本文）；需要銜接或用到某段的結果時，用 get_card 取該卡片的本文。"}))

;; ---------------------------------------------------------------------------
;; 寫入工具：讀目前狀態 → knot.ops（與畫面相同的規則）→ 一個修改組

(defn- uuid-arg
  "取出 UUID 參數；required? 為 true 時缺少就是錯誤，否則缺少或 null 為 nil。"
  [args k required?]
  (let [raw (get args k)]
    (cond
      (nil? raw) (when required? (tool-error! (str "缺少參數 " k)))
      (not (string? raw)) (tool-error! (str k " 格式不正確（應為 UUID 字串）"))
      :else (or (parse-uuid raw) (tool-error! (str k " 格式不正確（應為 UUID）：" raw))))))

(defn- enum-arg
  "取出列舉參數並轉成 keyword；不在 allowed 內為錯誤。"
  [args k allowed]
  (let [raw (get args k)
        v (when (string? raw) (keyword raw))]
    (cond
      (nil? raw) (tool-error! (str "缺少參數 " k))
      (some #{v} allowed) v
      :else (tool-error! (str k " 只接受 " (str/join "、" (map name allowed)) "，收到：" raw)))))

(defn- card-summary [nodes numbers id]
  {:card_id (str id)
   :kind (name (:node/kind (get nodes id)))
   :number (ops/heading-number nodes numbers id)
   :title (card-title nodes numbers id)})

(defn- block-by-id [{:keys [blocks]} id]
  (some #(when (= id (:block/id %)) %) blocks))

(defn- block-summary
  "素材的簡要資料（block_id、種類、名稱、來源、位置、狀態）。"
  [b]
  (cond-> {:block_id (str (:block/id b))
           :kind (name (:block/kind b))
           :title (:block/title b)
           :source (:block/source b)
           :status (name (or (:block/status b) :confirmed))}
    (:block/location b) (assoc :location (:block/location b))))

(defn- fact-entry
  "事實的資料：key、label、value、unit、所屬素材 block_id 與名稱、指定的列與欄。"
  [state {:fact/keys [key label value unit block row column]}]
  (let [b (block-by-id state block)]
    (cond-> {:key key :label label :value value :display (sources/fact-display {:fact/value value :fact/unit unit})
             :block_id (some-> block str)}
      unit (assoc :unit unit)
      b (assoc :block_title (:block/title b))
      (and b (sources/discarded? b)) (assoc :block_discarded true)
      (some? row) (assoc :row row :column column))))

(defn- write!
  "執行寫入工具：op 收目前狀態、回傳 knot.ops 的結果。
  錯誤與衝突以 isError 回報；成功時回傳修改組 id 與這次新增或修改的卡片（編號與名稱為寫入後的）。"
  [conn args op]
  (let [old (current-state conn)
        {:keys [error state label message note touched origin blocks facts citing fact-problems]} (op old)
        _ (when error (tool-error! error))
        changes (sync/diff (sync/data old) (sync/data state))]
    (if-not changes
      {:changeset_id nil :cards [] :message (or note "沒有任何變動：內容與目前相同")}
      (let [summary (let [s (get args "summary")] (when-not (str/blank? (when (string? s) s)) s))
            group (let [g (get args "group")] (when-not (str/blank? (when (string? g) g)) g))
            res (db/apply-changes! conn {:changes changes :by :agent :check-versions? true
                                         :origin origin :label (or summary label) :group group})
            nodes (:nodes state)
            numbers (tree/numbering nodes)]
        (when (:conflicts res) (tool-error! "卡片剛被修改，請重新讀取後再試"))
        (cond-> {:changeset_id (str (:changeset/id res))
                 :cards (vec (for [id (distinct touched) :when (contains? nodes id)]
                               (card-summary nodes numbers id)))
                 :message (or message (str "已" label "；所有修改都記錄在 Knot，可在畫面上檢視與復原"))}
          (seq blocks) (assoc :blocks (vec (for [id blocks :let [b (block-by-id state id)] :when b] (block-summary b))))
          (seq facts) (assoc :facts (vec (for [k facts :let [f (get (:facts state) k)] :when f] (fact-entry state f))))
          (seq citing) (assoc :cited_cards (vec (for [id citing] (card-summary nodes numbers id))))
          (seq fact-problems) (assoc :fact_problems (vec (for [{:keys [key error]} fact-problems] {:key key :error error}))))))))

(defn- add-card [conn args]
  (let [kind (enum-arg args "kind" [:heading :paragraph :intro :table :figure])]
    (when-not (contains? args "parent_id") (tool-error! "缺少參數 parent_id（新增一章時為 null）"))
    (write! conn args
            #(ops/add-card % {:kind kind
                              :parent-id (uuid-arg args "parent_id" false)
                              :after-id (uuid-arg args "after_id" false)
                              :title (get args "title")
                              :intent (get args "intent")
                              :label (get args "label")
                              :target-chars (get args "target_chars")}))))

(defn- parse-items
  "build_outline 的 items 轉成 knot.ops 的格式（kind 轉 keyword，其餘原樣；格式問題交給 ops 回報）。"
  [items]
  (if (sequential? items)
    (mapv (fn [item]
            (if (map? item)
              (cond-> {:kind (some-> (get item "kind") (as-> k (when (string? k) (keyword k))))
                       :title (get item "title")
                       :intent (get item "intent")
                       :label (get item "label")
                       :target-chars (get item "target_chars")}
                (contains? item "children") (assoc :children (parse-items (get item "children"))))
              item))
          items)
    items))

(defn- build-outline [conn args]
  (write! conn args
          #(ops/build-outline % {:parent-id (do (when-not (contains? args "parent_id")
                                                  (tool-error! "缺少參數 parent_id（新增章時為 null）"))
                                                (uuid-arg args "parent_id" false))
                                 :after-id (uuid-arg args "after_id" false)
                                 :items (parse-items (get args "items"))})))

(defn- update-card [conn args]
  (let [version (get args "version")]
    (when-not (or (nil? version) (integer? version)) (tool-error! "version 必須是整數"))
    (write! conn args
            #(ops/update-card % (merge {:id (card-id % args)
                                        :title (get args "title")
                                        :intent (get args "intent")
                                        :version version}
                                       ;; target_chars 傳 null 表示清除，要與沒傳區分
                                       (when (contains? args "target_chars")
                                         {:target-chars (get args "target_chars")}))))))

(defn- version-arg
  "取出選填的 version 參數（整數）。"
  [args]
  (let [version (get args "version")]
    (when-not (or (nil? version) (integer? version)) (tool-error! "version 必須是整數"))
    version))

(def ^:private max-hints 20)

(defn- hint-list
  "提示用的選項清單：最多列 max-hints 個，超過時註明總數。"
  [items]
  (str (str/join "、" (take max-hints items))
       (when (> (count items) max-hints) (str "…（共 " (count items) " 個）"))))

(defn- marker-problems
  "本文標記（knot.prose/markup->doc 的 :refs）中不存在的事實、label、區塊：一次全部列出並附上可用的選項。
  都存在時為 nil。"
  [{:keys [nodes facts blocks]} refs]
  (let [fact-keys (set (keys facts))
        labelled (filter :node/label (vals nodes))
        labels (set (map :node/label labelled))
        usable (remove sources/discarded? blocks)
        block-ids (set (map :block/id usable))
        discarded-ids (set (map :block/id (filter sources/discarded? blocks)))
        bad-facts (remove fact-keys (:facts refs))
        bad-labels (remove labels (:labels refs))
        bad-blocks (remove #(contains? block-ids (parse-uuid %)) (:blocks refs))
        gone-blocks (filter #(contains? discarded-ids (parse-uuid %)) bad-blocks)]
    (when (or (seq bad-facts) (seq bad-labels) (seq bad-blocks))
      (str/join "\n"
                (concat
                 (when (seq bad-facts)
                   [(str "事實不存在：" (str/join "、" bad-facts) "。"
                         (if (empty? facts)
                           "目前報告還沒有事實，數字先直接寫在本文中（沒有事實時 Knot 不會提醒未綁定的數字）。"
                           (str "可用的事實 key：" (hint-list (for [[k f] (sort-by key facts)]
                                                          (str k "（" (:fact/label f) "）"))) "。")))])
                 (when (seq bad-labels)
                   [(str "label 不存在：" (str/join "、" bad-labels) "。"
                         (if (empty? labelled)
                           "目前報告還沒有表格或圖：請先用 add_card 在這段底下建立（可用 label 參數指定代稱），再用 {{ref:label}} 引用。"
                           (str "可用的 label：" (hint-list (for [n labelled]
                                                          (str (:node/label n) "（" (:node/title n) "）"))) "。")))])
                 (when (seq bad-blocks)
                   [(str "素材 id 不存在或已捨棄：" (str/join "、" bad-blocks) "。"
                         (when (seq gone-blocks) (str "其中 " (str/join "、" gone-blocks) " 已被捨棄。"))
                         (if (empty? usable)
                           "目前報告還沒有可引用的素材；用 search_sources 查詢，沒有就用 add_excerpt 或 add_source_table 新增。"
                           (str "可引用的素材（id（名稱））：" (hint-list (for [b usable]
                                                                (str (:block/id b) "（" (:block/title b) "）"))) "。")))]))))))

(defn- write-prose [conn args]
  (let [version (version-arg args)
        state (current-state conn)
        id (card-id state args)
        text (get args "text")
        _ (when-not (string? text) (tool-error! "缺少參數 text（本文，每行一段）"))
        {:keys [doc refs errors]} (prose/markup->doc text)
        _ (when errors (tool-error! (str "本文格式不正確：\n" (str/join "\n" errors))))
        _ (when-let [why (marker-problems state refs)] (tool-error! (str "本文的標記有問題，沒有寫入：\n" why)))
        numbers (prose/unbound-numbers (:facts state) doc)
        res (write! conn args
                    #(ops/write-prose % {:id id :doc doc :version version
                                         :cite-blocks (mapv parse-uuid (:blocks refs))}))]
    (cond-> (assoc res :unbound_numbers numbers)
      (seq numbers)
      (update :message str "；本文有 " (count numbers) " 個數字沒有綁定事實（" (str/join "、" numbers)
              "），會在畫面上提醒使用者；確定是事實時請改用 {{fact:key}}"))))

(defn- write-table [conn args]
  (let [version (version-arg args)]
    (write! conn args
            #(ops/write-table % {:id (card-id % args)
                                 :columns (get args "columns")
                                 :rows (get args "rows")
                                 :version version}))))

(defn- move-card [conn args]
  (let [position (enum-arg args "position" [:before :after :into])]
    (write! conn args
            #(ops/move-card % {:id (card-id % args)
                               :target-id (uuid-arg args "target_id" true)
                               :position position}))))

(defn- set-relation [conn args]
  (let [type (enum-arg args "type" tree/adjacent-relation-types)]
    (write! conn args
            #(ops/set-relation % {:from-id (uuid-arg args "from_id" true)
                                  :to-id (uuid-arg args "to_id" true)
                                  :type type}))))

(defn- add-remote-relation [conn args]
  (let [type (enum-arg args "type" tree/remote-relation-types)]
    (write! conn args
            #(ops/add-remote-relation % {:from-id (uuid-arg args "from_id" true)
                                         :to-id (uuid-arg args "to_id" true)
                                         :type type}))))

(defn- delete-card [conn args]
  (let [version (get args "version")]
    (when-not (or (nil? version) (integer? version)) (tool-error! "version 必須是整數"))
    (write! conn args #(ops/delete-card % {:id (card-id % args) :version version}))))

(defn- remove-relation [conn args]
  (let [type (when (some? (get args "type"))
               (enum-arg args "type" (distinct (concat tree/adjacent-relation-types tree/remote-relation-types))))]
    (write! conn args
            #(ops/remove-relation % {:from-id (uuid-arg args "from_id" true)
                                     :to-id (uuid-arg args "to_id" true)
                                     :type type}))))

(defn- update-report [conn args]
  (let [title (get args "title") subtitle (get args "subtitle")]
    (when (and (nil? title) (nil? subtitle)) (tool-error! "請至少給 title 或 subtitle"))
    (when (and (some? title) (or (not (string? title)) (str/blank? title)))
      (tool-error! "title 必須是非空白的字串"))
    (when (and (some? subtitle) (not (string? subtitle))) (tool-error! "subtitle 必須是字串"))
    (db/update-report! conn (cond-> {}
                              (some? title) (assoc :report/title (str/trim title))
                              (some? subtitle) (assoc :report/subtitle (when-not (str/blank? subtitle) (str/trim subtitle)))))
    (let [r (db/report-info (d/db conn))]
      {:report {:title (:report/title r) :subtitle (:report/subtitle r)}
       :message "已更新報告資訊；不屬於內容修改，不產生修改組"})))

;; ---------------------------------------------------------------------------
;; 素材與事實（SPEC 5.2、5.5）

(defn- add-excerpt [conn args]
  (write! conn args
          #(source-ops/add-excerpt % {:source (get args "source") :title (get args "title") :text (get args "text")
                                      :location (get args "location") :note (get args "note")})))

;; 批次：每一項的欄位同單筆工具；項目內的格式問題（例如 block_id 不是 UUID）併入該筆的錯誤，不中斷整批檢查
(defn- add-excerpts [conn args]
  (let [items (get args "excerpts")]
    (write! conn args
            #(source-ops/add-excerpts
              % {:excerpts (when (sequential? items)
                             (for [it items]
                               (if (map? it)
                                 {:source (get it "source") :title (get it "title") :text (get it "text")
                                  :location (get it "location") :note (get it "note")}
                                 it)))}))))

(defn- add-facts [conn args]
  (let [items (get args "facts")]
    (write! conn args
            #(source-ops/add-facts
              % {:facts (when (sequential? items)
                          (for [it items]
                            (if (map? it)
                              (let [b (get it "block_id")]
                                {:key (get it "key") :label (get it "label") :value (get it "value")
                                 :unit (get it "unit") :row (get it "row") :column (get it "column")
                                 :block-id (if (string? b) (or (parse-uuid b) b) b)})
                              it)))}))))

(defn- add-source-table [conn args]
  (write! conn args
          #(source-ops/add-source-table % {:source (get args "source") :title (get args "title")
                                           :location (get args "location") :note (get args "note")
                                           :columns (get args "columns") :rows (get args "rows")})))

(defn- update-source-block [conn args]
  (let [id (uuid-arg args "block_id" true)]
    (write! conn args
            #(source-ops/update-source-block % {:id id :title (get args "title") :text (get args "text")
                                                :location (get args "location") :note (get args "note")
                                                :columns (get args "columns") :rows (get args "rows")}))))

(defn- discard-source-block [conn args]
  (let [id (uuid-arg args "block_id" true)]
    (write! conn args #(source-ops/discard-source-block % {:id id}))))

(defn- add-fact [conn args]
  (write! conn args
          #(source-ops/add-fact % {:key (get args "key") :label (get args "label") :value (get args "value")
                                   :unit (get args "unit") :block-id (uuid-arg args "block_id" false)
                                   :row (get args "row") :column (get args "column")})))

(defn- update-fact [conn args]
  (let [block-id (uuid-arg args "block_id" false)]
    (write! conn args
            #(source-ops/update-fact % (cond-> {:key (get args "key") :label (get args "label") :value (get args "value")
                                                :unit (get args "unit") :block-id block-id}
                                         ;; row 與 column 傳 null 表示改為不指定儲存格，要與沒傳區分
                                         (contains? args "row") (assoc :row (get args "row"))
                                         (contains? args "column") (assoc :column (get args "column")))))))

(defn- delete-fact [conn args]
  (let [k (get args "key")]
    (when-not (string? k) (tool-error! "缺少參數 key（字串）"))
    (write! conn args #(source-ops/delete-fact % {:key k}))))

(defn- add-citation [conn args]
  (let [block-id (uuid-arg args "block_id" true)]
    (write! conn args #(source-ops/add-citation % {:id (card-id % args) :block-id block-id :where (get args "where")}))))

(defn- remove-citation [conn args]
  (let [block-id (uuid-arg args "block_id" true)]
    (write! conn args #(source-ops/remove-citation % {:id (card-id % args) :block-id block-id}))))

(def ^:private excerpt-chars 120)
(def ^:private default-search-limit 20)
(def ^:private max-search-limit 100)

(defn- fact-counts [{:keys [facts]}]
  (frequencies (map :fact/block (vals facts))))

(defn- search-entry [counts b]
  (cond-> (assoc (block-summary b) :fact_count (get counts (:block/id b) 0))
    (sources/table? b) (assoc :columns (:table/columns b) :row_count (count (:table/rows b)))
    (not (sources/table? b)) (assoc :excerpt (truncate (:block/text b) excerpt-chars))
    (:block/note b) (assoc :note (:block/note b))))

(defn- search-sources [conn args]
  (let [state (current-state conn)
        query (let [q (get args "query")]
                (cond (nil? q) ""
                      (string? q) q
                      :else (tool-error! "query 必須是字串（空字串表示列出全部）")))
        kind (when (some? (get args "kind")) (enum-arg args "kind" [:para :table]))
        source (let [s (get args "source")]
                 (when (some? s) (if (string? s) s (tool-error! "source 必須是來源檔名字串"))))
        limit (let [l (get args "limit")]
                (cond (nil? l) default-search-limit
                      (and (integer? l) (<= 1 l max-search-limit)) l
                      :else (tool-error! (str "limit 必須是 1 到 " max-search-limit " 的整數"))))
        found (sources/search (:blocks state) query {:kind kind :source source})
        counts (fact-counts state)]
    (cond-> {:total (count found)
             :results (mapv #(search-entry counts %) (take limit found))
             :sources (mapv (fn [{:keys [source count]}] {:source source :count count})
                            (sources/source-counts (:blocks state)))}
      (> (count found) limit) (assoc :message (str "只列出前 " limit " 筆（共 " (count found) " 筆）；"
                                                   "加入更多關鍵字、用 kind 或 source 篩選，或調高 limit"))
      (empty? found) (assoc :message (str "沒有符合的素材；查詢以空白分成多個詞，素材要同時包含所有詞，"
                                          "可減少關鍵字。已有的素材來源見 sources")))))

(defn- block-id-arg
  "取出 block_id 並確認素材存在。"
  [state args]
  (let [id (uuid-arg args "block_id" true)]
    (if (block-by-id state id)
      id
      (tool-error! (str "找不到素材：" id "。請先用 search_sources 查詢有效的 block_id")))))

(defn- get-source-block [conn args]
  (let [{:keys [nodes] :as state} (current-state conn)
        id (block-id-arg state args)
        b (block-by-id state id)
        numbers (tree/numbering nodes)
        cited (for [nid (tree/document-order nodes)
                    :when (some #(= id (:block %)) (get-in nodes [nid :node/citations]))]
                (assoc (card-summary nodes numbers nid)
                       :position (tree/position-label nodes numbers nid)))]
    (cond-> (assoc (block-summary b)
                   :facts (mapv #(fact-entry state %) (sort-by :fact/key (filter #(= id (:fact/block %)) (vals (:facts state)))))
                   :cited_by (vec cited))
      (:block/note b) (assoc :note (:block/note b))
      (sources/table? b) (assoc :columns (:table/columns b) :rows (:table/rows b))
      (not (sources/table? b)) (assoc :text (:block/text b)))))

(defn- list-facts [conn args]
  (let [state (current-state conn)
        only (when (some? (get args "block_id")) (block-id-arg state args))
        facts (cond->> (sort-by :fact/key (vals (:facts state)))
                only (filter #(= only (:fact/block %))))]
    {:facts (mapv #(fact-entry state %) facts)}))

;; ---------------------------------------------------------------------------
;; 留言與待辦：不是內容修改，不產生修改組

(defn- reply-entries [replies]
  (vec (for [r replies]
         {:author (name (:note/author r)) :text (:note/text r) :at (inst-str (:note/at r))})))

(defn- note-card
  "留言所屬卡片的摘要；整份報告的留言為 nil。"
  [nodes numbers note]
  (when-let [id (:note/node note)]
    (when (contains? nodes id) (card-summary nodes numbers id))))

(defn- list-todos [conn args]
  (let [{:keys [nodes] :as state} (current-state conn)
        numbers (tree/numbering nodes)
        db (d/db conn)
        only (when (some? (get args "card_id"))
               (card-id state args))
        wanted? #(or (nil? only) (= only (:note/node %)))]
    {:todos (vec (for [t (notes/open-todos db) :when (wanted? t)]
                   {:note_id (str (:note/id t))
                    :card (note-card nodes numbers t)
                    :text (:note/text t)
                    :at (inst-str (:note/at t))
                    :replies (reply-entries (notes/thread db (:note/id t)))}))
     :questions_answered
     (vec (for [q (notes/agent-threads db) :when (wanted? q)
                :let [replies (notes/thread db (:note/id q))]
                :when (= :user (:note/author (last replies)))]
            {:note_id (str (:note/id q))
             :card (note-card nodes numbers q)
             :question (:note/text q)
             :replies (reply-entries replies)}))}))

(defn- add-note [conn args]
  (when-not (contains? args "card_id") (tool-error! "缺少參數 card_id（整份報告的留言為 null）"))
  (let [node (uuid-arg args "card_id" false)
        reply-to (uuid-arg args "reply_to" false)
        res (notes/add-note! conn {:node node :author :agent :text (get args "text") :reply-to reply-to})]
    (when-let [msg (:error res)] (tool-error! msg))
    {:note_id (str (:note/id res))
     :message (if reply-to "已回覆；使用者會在 Knot 畫面上看到" "已留言；使用者會在 Knot 畫面上看到")}))

(defn- complete-todo [conn args]
  (let [id (uuid-arg args "note_id" true)
        db (d/db conn)
        todo (some #(when (= id (:note/id %)) %) (notes/list-notes db))
        _ (cond
            (nil? todo) (tool-error! (str "找不到留言：" id))
            (not (:note/todo todo)) (tool-error! "這則留言不是待辦")
            (= :done (:note/state todo)) (tool-error! "這個待辦已經完成"))
        reply (get args "reply")
        _ (when (and (some? reply) (or (not (string? reply)) (str/blank? reply)))
            (tool-error! "reply 不能是空白"))
        cs-id (or (uuid-arg args "changeset_id" false)
                  (notes/latest-agent-changeset db (:note/node todo) (:note/at todo)))
        res (notes/set-state! conn id :done {:changeset cs-id})]
    (when-let [msg (:error res)] (tool-error! msg))
    (when reply
      (let [r (notes/add-note! conn {:author :agent :text reply :reply-to id})]
        (when-let [msg (:error r)] (tool-error! msg))))
    {:message "待辦已標為完成" :changeset_id (some-> cs-id str)}))

;; ---------------------------------------------------------------------------
;; 工具登記

(def id-schema
  {:type "object"
   :properties {:card_id {:type "string" :description "卡片 id（UUID，由 get_outline 的 card_id 取得）"}}
   :required ["card_id"]
   :additionalProperties false})

(def read-tools
  [{:name "get_outline"
    :description "取得整份報告的卡片樹（依文件順序的扁平清單）與報告資訊（名稱 title 與副標 subtitle，可用 update_report 修改）：card_id、類型、深度、編號 number、名稱 title、意圖摘要、狀態、是否有本文，字數 chars（段落、引言為本文去掉空白的字數，標題為整章／整節合計，表格與圖沒有）與目標字數 target_chars（有設定才有），以及未完成待辦數 open_todos（沒有待辦時省略）。title 是純名稱，不含編號；編號（標題為「第一章」「1.2」，其他卡片為 null）由卡片的位置決定、隨移動重新計算，存在 number 欄位，不要把編號寫進名稱。這是規劃與定位卡片的起點；意圖預設截斷為前 80 字；要完整意圖時傳 full_intent: true（撰寫或審閱整份大綱時用，不必逐張 get_card）。"
    :inputSchema {:type "object"
                  :properties {:full_intent {:type "boolean" :description "true 時回傳完整意圖（預設 false：前 80 字）"}}
                  :additionalProperties false}
    :handler get-outline}
   {:name "get_card"
    :description "取得單張卡片的完整資料：完整意圖、字數 chars 與目標字數 target_chars（有設定才有）、本文 prose（與 write_prose 相同的格式：每行一段、「- 」與「1. 」開頭的行是清單、事實以 {{fact:key}}、交互參照以 {{ref:label}}、引用以 [[cite:區塊id]] 表示，修改後可直接交回）與其中未綁定事實的數字 unbound_numbers、表格卡片的 label 與內容 table（欄名 columns 與各列 rows；沒有內容時省略）、圖的 label、所在章節路徑 path（每層為 {number, title}，title 不含編號）、依據的素材、前後段與其他段落的關係、下層卡片（相鄰、遠距關係與下層卡片的 title 同樣是純名稱，編號在 number）、這張卡片上的留言與待辦（notes），以及最近 10 筆修改紀錄 history（舊到新；每筆有時間、修改者 user 或 agent、修改組名稱、類型、改了哪些欄位、是否已復原，名稱與意圖有變時附 text_changes 的前後文字，不含本文全文）。history 讓你知道使用者改了什麼，例如處理待辦前先看，避免蓋掉使用者剛改的內容。要看某張卡片的細節或確認它的關係時使用；card_id 由 get_outline 取得。"
    :inputSchema id-schema
    :handler get-card}
   {:name "get_cards"
    :description "一次取得多張卡片（最多 30 張），每張的內容與 get_card 相同。要連續看多張卡片時用它，不必逐張呼叫 get_card。找不到或格式不對的 card_id 列在 errors，不影響其他卡片。"
    :inputSchema {:type "object"
                  :properties {:card_ids {:type "array" :items {:type "string"}
                                          :description "卡片 id 的陣列（UUID，由 get_outline 的 card_id 取得），依序回傳"}}
                  :required ["card_ids"]
                  :additionalProperties false}
    :handler get-cards}
   {:name "get_writing_context"
    :description "取得撰寫某段本文時應參考的內容（章節路徑、本段意圖、依據的素材與事實、前後段、有關係的段落、文風、本段的目標字數 target_chars），與 Knot 編輯區的「撰寫時參考的內容」相同。周邊卡片（上層、前後段、有關係的段落）只帶意圖，has_prose 為 true 表示對方已有本文，需要銜接或用到其結果時用 get_card 取它的本文；引言例外，下層內容有本文時帶本文。撰寫本文時以它為依據，並參考目標字數控制篇幅；只有段落與引言適用。context_chars 是這份參考內容本身的長度，不是本文的字數。"
    :inputSchema id-schema
    :handler get-writing-context}
   {:name "list_todos"
    :description "列出使用者交代的待辦與使用者的回覆。todos＝尚未完成的待辦（含所屬卡片，整份報告的待辦 card 為 null，以及討論串中的回覆）；questions_answered＝你先前用 add_note 提問、使用者已回答但你尚未接話的討論串：討論串最後一則是使用者的回覆就算沒接話，處理完要用 add_note（reply_to 指向那則提問）在同一串回一句說明你怎麼處理，之後它才不會再出現。開始工作時先呼叫；處理完待辦用 complete_todo。card_id 選填，只列出某張卡片的。待辦與提問各以 note_id 識別。"
    :inputSchema {:type "object"
                  :properties {:card_id {:type "string" :description "只列出這張卡片的（選填，UUID）"}}
                  :additionalProperties false}
    :handler list-todos}])

(def ^:private summary-property
  {:type "string" :description "這次修改的名稱（選填），會顯示在 Knot 的復原提示；省略時由 Knot 依操作產生"})

(def ^:private group-property
  {:type "string" :description "把連續的多次寫入算成同一次修改（選填）：一次工作中的寫入都傳相同的 group（例如「依審查意見調整大綱」），Knot 的歷程顯示為一筆、使用者按 Ctrl+Z 一次復原整組。group 也是這次修改在畫面上顯示的名稱。中間有使用者或其他來源的寫入、相隔超過 30 分鐘或組名不同時會另起一組。"})

(def ^:private uuid-property {:type "string" :description "卡片 id（UUID，由 get_outline 的 card_id 取得）"})

(def ^:private outline-item-schema
  {:type "object"
   :properties {:kind {:type "string" :enum ["heading" "paragraph" "intro" "table" "figure"]}
                :title {:type "string" :description "標題名稱、段落標題或表格與圖的標題；引言不需要"}
                :intent {:type "string" :description "意圖：這張卡片要寫什麼"}
                :target_chars {:type "integer" :minimum 1 :description "目標字數（選填，正整數）；標題為整章／整節的目標，只用於標題、段落、引言"}
                :label {:type "string" :description "表格與圖的 label 代稱（選填），小寫英數與連字號，例如 ridership；label 會是 tab:ridership 或 fig:ridership。省略時自動產生"}
                :children {:type "array" :description "下層卡片，格式同本項，可再巢狀：標題底下可以是引言、段落、下層標題，段落底下只能是表格與圖"
                           :items {:type "object"}}}
   :required ["kind"]})

(def write-tools
  [{:name "add_card"
    :description "新增一張卡片（標題、段落、引言、表格或圖）。規則同 Knot 畫面：引言每個標題最多一張且在最前面；標題底下依序是引言、這一層的段落、下層標題（段落與下層標題可以並存，但段落一律排在下層標題之前，不能排在下層標題之後）；表格（table）與圖（figure）只能掛在段落底下：parent_id 是該段落的 card_id，title 是表圖的標題、intent 是它要呈現什麼；建立時會產生 label（tab:代稱 或 fig:代稱，建立後不變、報告內不可重複），可用 label 參數給代稱（小寫英文字母、數字與連字號，例如 ridership），省略時自動產生 4 碼短 id；段落本文用 {{ref:label}} 引用它，表格內容之後用 write_table 填寫。標題最多四層。parent_id 為 null 表示新增一章。after_id 指定接在同一層的哪張卡片之後；省略時引言放最前面、段落放在最後一個段落之後（下層標題之前）、標題放最後。新段落的狀態為 none，本文之後再寫。可用 target_chars 設定目標字數（標題、段落、引言）。不合法時會回傳具體原因，不會寫入。一次要建立多張卡片請用 build_outline。"
    :inputSchema {:type "object"
                  :properties {:kind {:type "string" :enum ["heading" "paragraph" "intro" "table" "figure"]
                                      :description "heading＝標題、paragraph＝段落、intro＝引言、table＝表格、figure＝圖"}
                               :parent_id {:type ["string" "null"] :description "上層標題的 id；null 表示新增一章"}
                               :after_id {:type "string" :description "同一層的某張卡片 id，新卡片接在它後面（選填）"}
                               :title {:type "string" :description "標題名稱、段落標題或表格與圖的標題；引言不需要"}
                               :intent {:type "string" :description "意圖：這張卡片要寫什麼（選填）"}
                               :target_chars {:type "integer" :minimum 1 :description "目標字數（選填，正整數）：本文預期的篇幅，標題為整章／整節的目標；只用於標題、段落、引言"}
                               :label {:type "string" :description "表格與圖的 label 代稱（選填）：小寫英文字母、數字與連字號，例如 ridership，label 即為 tab:ridership／fig:ridership；省略時自動產生；只用於表格與圖"}
                               :summary summary-property
                               :group group-property}
                  :required ["kind" "parent_id"]
                  :additionalProperties false}
    :handler add-card}
   {:name "build_outline"
    :description "一次建立巢狀的報告骨架（標題、段落、引言、表格、圖），整批寫成一個修改組，使用者可一次復原。規劃章節結構時使用。任何一處不合法就整批不寫，錯誤會指出是哪一項（例如 items[1].children[0]）與原因。規則與 add_card 相同；標題的 children 是引言、段落、下層標題，段落的 children 只能是表格與圖（可用 label 給代稱，同批的 label 也不能重複）；標題、段落、引言可用 target_chars 設定目標字數；同一個標題底下的 items 順序必須是引言、段落、下層標題（段落排在下層標題之前，可以並存）。回傳所有新卡片（依文件順序，含編號與 card_id），之後可用 card_id 繼續操作。"
    :inputSchema {:type "object"
                  :properties {:parent_id {:type ["string" "null"] :description "放在哪個標題底下；null 表示放在最上層（建立章）"}
                               :after_id {:type "string" :description "同一層的某張卡片 id，第一項接在它後面（選填，省略時放最後）"}
                               :items {:type "array" :description "要建立的卡片，依序排列" :items outline-item-schema}
                               :summary summary-property
                               :group group-property}
                  :required ["parent_id" "items"]
                  :additionalProperties false}
    :handler build-outline}
   {:name "write_prose"
    :description "寫入段落或引言的本文（表格請用 write_table；標題與圖沒有本文）。text 的格式：每一行是一段（空行忽略）；以「- 」（或「* 」「• 」）開頭的連續行是項目清單，以「1. 」這類數字加半形句點開頭的連續行是編號清單（編號由 Knot 依順序產生），清單只有一層。引用用標記：{{fact:事實key}}（事實，畫面顯示值與單位）、{{ref:label}}（交互參照表格或圖，label 如 tab:ridership）、[[cite:區塊id]]（引用素材區塊，同時加入這段的依據）；標記中不存在的事實、label、區塊會一次全部列出並拒絕寫入。本文中不在標記內的數字會回傳在 unbound_numbers，並在畫面上提醒使用者；有事實可用時請用 {{fact:key}}。寫入後狀態轉為 ai（引言不套用狀態）。不論目標原本是不是使用者寫的（human）都會直接覆寫，但使用者可以比較與復原，所以修改既有本文前先用 get_card 看 prose 與 history，在原本的基礎上改。傳入 get_card 的 version 可避免蓋掉使用者剛做的修改。"
    :inputSchema {:type "object"
                  :properties {:card_id uuid-property
                               :text {:type "string" :description "本文，格式如上（get_card 的 prose 就是同樣的格式）"}
                               :version {:type "integer" :description "讀取時的卡片版本（選填）；與目前版本不同時拒絕寫入"}
                               :summary summary-property
                               :group group-property}
                  :required ["card_id" "text"]
                  :additionalProperties false}
    :handler write-prose}
   {:name "write_table"
    :description "寫入表格卡片自己的內容（欄名與各列），例如自行整理的指標總表；只有表格卡片能用，狀態轉為 ai。columns 是非空的欄名陣列，rows 是列的陣列（可為空陣列），每一列的儲存格數必須等於欄數，儲存格是字串或數字（數字轉成字串）。會取代原本的內容。表格的儲存格不列入「未綁定的數字」。表格先用 add_card 建在段落底下，段落本文用 {{ref:label}} 引用它。"
    :inputSchema {:type "object"
                  :properties {:card_id uuid-property
                               :columns {:type "array" :items {:type "string"} :description "欄名"}
                               :rows {:type "array" :description "各列；每列是與 columns 等長的儲存格陣列"
                                      :items {:type "array" :items {:type ["string" "number"]}}}
                               :version {:type "integer" :description "讀取時的卡片版本（選填）；與目前版本不同時拒絕寫入"}
                               :summary summary-property
                               :group group-property}
                  :required ["card_id" "columns" "rows"]
                  :additionalProperties false}
    :handler write-table}
   {:name "update_card"
    :description "修改卡片的名稱、意圖或目標字數（標題的名稱、段落的標題、卡片的意圖、target_chars）；不修改本文。傳入 get_card 取得的 version 可避免覆蓋使用者剛做的修改：版本不同時不會寫入。引言沒有名稱。"
    :inputSchema {:type "object"
                  :properties {:card_id uuid-property
                               :title {:type "string" :description "新的標題名稱（標題）或段落標題（段落）"}
                               :intent {:type "string" :description "新的意圖，會取代原本的內容"}
                               :target_chars {:type ["integer" "null"] :minimum 1 :description "目標字數（正整數）；傳 null 清除；不傳則不改。只用於標題、段落、引言"}
                               :version {:type "integer" :description "讀取時的卡片版本（選填）；與目前版本不同時拒絕寫入"}
                               :summary summary-property
                               :group group-property}
                  :required ["card_id"]
                  :additionalProperties false}
    :handler update-card}
   {:name "move_card"
    :description "移動卡片到另一張卡片的前面、後面或裡面，規則與使用者在畫面拖曳相同（層級、引言在最前、段落排在下層標題之前、最多四層）。移動段落後，受影響的相鄰段落關係會標為「待確認」，請使用者或之後用 set_relation 確認；標題移動後編號會重新計算。不合法時回傳原因。"
    :inputSchema {:type "object"
                  :properties {:card_id {:type "string" :description "要移動的卡片 id"}
                               :target_id {:type "string" :description "放置位置參考的卡片 id"}
                               :position {:type "string" :enum ["before" "after" "into"]
                                          :description "before＝放在目標之前、after＝放在目標之後、into＝放進目標底下（引言放最前面；段落放在最後一個段落之後、下層標題之前；標題放最後）"}
                               :summary summary-property
                               :group group-property}
                  :required ["card_id" "target_id" "position"]
                  :additionalProperties false}
    :handler move-card}
   {:name "set_relation"
    :description "設定兩個相鄰段落（同一標題下前後相接，from 在前）的關係並確認（取消「待確認」）。不相鄰或不是段落時回錯誤；不相鄰的段落請用 add_remote_relation。"
    :inputSchema {:type "object"
                  :properties {:from_id {:type "string" :description "前一個段落的 id"}
                               :to_id {:type "string" :description "緊接在後的段落 id"}
                               :type {:type "string" :enum ["progression" "causal" "contrast" "example"]
                                      :description "progression＝遞進、causal＝因果、contrast＝對比、example＝舉例"}
                               :summary summary-property
                               :group group-property}
                  :required ["from_id" "to_id" "type"]
                  :additionalProperties false}
    :handler set-relation}
   {:name "add_remote_relation"
    :description "新增兩個段落之間不相鄰的關係（任意距離）：from 為本段、to 為目標段落。撰寫本段時，目標段落的內容會成為參考。已存在相同關係時不會重複新增。"
    :inputSchema {:type "object"
                  :properties {:from_id {:type "string" :description "本段的 id"}
                               :to_id {:type "string" :description "目標段落的 id"}
                               :type {:type "string" :enum ["summarizes" "basis" "echo" "contrast"]
                                      :description "summarizes＝摘要（本段彙整目標的結果）、basis＝依據（本段論點建立在目標的結果上）、echo＝呼應、contrast＝對比"}
                               :summary summary-property
                               :group group-property}
                  :required ["from_id" "to_id" "type"]
                  :additionalProperties false}
    :handler add-remote-relation}
   {:name "remove_relation"
    :description "移除兩個段落之間的關係（相鄰或不相鄰皆可），from_id、to_id 的方向與建立時相同。給 type 時只移除該類型，省略時移除這兩段之間 from → to 的所有關係。沒有符合的關係時回錯誤。現有的關係可用 get_card 查看。"
    :inputSchema {:type "object"
                  :properties {:from_id {:type "string" :description "關係的起點段落 id"}
                               :to_id {:type "string" :description "關係的目標段落 id"}
                               :type {:type "string"
                                      :enum ["progression" "causal" "contrast" "example" "summarizes" "basis" "echo"]
                                      :description "只移除這個類型（選填）"}
                               :summary summary-property
                               :group group-property}
                  :required ["from_id" "to_id"]
                  :additionalProperties false}
    :handler remove-relation}
   {:name "delete_card"
    :description "刪除一張卡片與它所有的下層卡片（例如刪除標題會一併刪除底下的段落），相關的關係也一併移除；原本接在後面的段落改為承接前一段，關係標為「待確認」。刪除會記錄成一個修改組，使用者可在 Knot 畫面復原。刪除前請用 get_card 確認內容；傳入 version 可避免刪掉使用者剛修改的卡片。報告至少要保留一張卡片。"
    :inputSchema {:type "object"
                  :properties {:card_id uuid-property
                               :version {:type "integer" :description "讀取時的卡片版本（選填）；與目前版本不同時拒絕刪除"}
                               :summary summary-property
                               :group group-property}
                  :required ["card_id"]
                  :additionalProperties false}
    :handler delete-card}
   {:name "add_note"
    :description "在卡片上留言給使用者，或回覆討論串。意圖不清楚、素材互相矛盾、需要使用者決定時用它提問，而不是自己猜；也用於回覆使用者的留言。reply_to 給被回覆的留言 note_id 時，留言會接在該討論串且沿用它的卡片（card_id 可省略但若給了必須相同）；card_id 為 null 表示整份報告的留言。留言不是內容修改，不會產生修改組，也不能建立待辦。之後可用 list_todos 的 questions_answered 看使用者的回答。"
    :inputSchema {:type "object"
                  :properties {:card_id {:type ["string" "null"] :description "留言所屬卡片的 id；null 表示整份報告"}
                               :text {:type "string" :description "留言內容"}
                               :reply_to {:type "string" :description "被回覆的留言 id（選填）"}}
                  :required ["card_id" "text"]
                  :additionalProperties false}
    :handler add-note}
   {:name "complete_todo"
    :description "把使用者的待辦標為完成。changeset_id 選填：省略時 Knot 會連結待辦建立之後、你對該卡片所做的最近一個修改組（沒有則不連結）。reply 選填，會以你的身分回覆在該待辦底下，用來說明做了什麼。不是待辦或已完成時回錯誤。完成待辦本身不是內容修改，不會產生修改組。"
    :inputSchema {:type "object"
                  :properties {:note_id {:type "string" :description "待辦的留言 id（由 list_todos 的 note_id 取得）"}
                               :reply {:type "string" :description "完成後的說明（選填）"}
                               :changeset_id {:type "string" :description "完成待辦所做的修改組 id（選填，寫入工具回傳的 changeset_id）"}}
                  :required ["note_id"]
                  :additionalProperties false}
    :handler complete-todo}
   {:name "update_report"
    :description "修改報告的名稱與副標，顯示在 Knot 頂部列與匯出。title、subtitle 都選填但至少給一個；subtitle 給空字串表示清除副標。這不是內容修改，不產生修改組，也不能復原。目前的名稱與副標可由 get_outline 的 report 讀到。"
    :inputSchema {:type "object"
                  :properties {:title {:type "string" :description "報告名稱（不可為空白）"}
                               :subtitle {:type "string" :description "副標；空字串表示清除"}}
                  :additionalProperties false}
    :handler update-report}])

(def ^:private block-id-property
  {:type "string" :description "素材 id（UUID，由 search_sources 或 add_excerpt／add_source_table 的 block_id 取得）"})

(def ^:private source-property
  {:type "string" :description "來源檔名稱，例如「期中報告.pdf」；Knot 不保存原始檔，只記下名稱讓使用者回頭核對。同一份資料請固定用同一個名稱"})

(def ^:private location-property
  {:type "string" :description "在來源中的位置（選填），例如「p.12」「3.2 節」「工作表 2」"})

(def ^:private note-property
  {:type "string" :description "用途說明（選填）：為什麼節錄這一段、預計用在哪裡"})

(def ^:private verbatim-rule
  "節錄必須照抄原文：不改字、不改數字、不改標點，不要摘要、改寫或翻譯；一段只講一件事（通常是幾句話）。")

(def source-tools
  [{:name "search_sources"
    :description (str "搜尋素材庫。query 以空白分成多個詞，素材的名稱、原文、表格儲存格、來源檔名（NFKC 正規化、不分大小寫）同時包含所有詞才符合；空字串列出全部。可用 kind（para 節錄／table 表格）、source（來源檔名，完全相同）篩選，已捨棄的素材不會出現。結果依相關度排序（詞出現在名稱的優先），每筆有 block_id、種類、名稱、來源、位置、原文前 " excerpt-chars " 字（表格為欄名 columns 與列數 row_count）、用途說明與事實數 fact_count；total 是符合的總數，limit 預設 " default-search-limit "。回傳的 sources 列出所有來源檔名與素材數，讓你知道已經有哪些素材。新增素材（add_excerpt、add_source_table）前先用它查詢，避免重複。要看完整原文或整張表用 get_source_block。")
    :inputSchema {:type "object"
                  :properties {:query {:type "string" :description "搜尋詞，以空白分隔（全部都要出現）；空字串列出全部"}
                               :kind {:type "string" :enum ["para" "table"] :description "只找節錄或表格（選填）"}
                               :source {:type "string" :description "只找這個來源檔名的素材（選填）"}
                               :limit {:type "integer" :minimum 1 :maximum max-search-limit :description (str "最多回傳幾筆（選填，預設 " default-search-limit "）")}}
                  :required ["query"]
                  :additionalProperties false}
    :handler search-sources}
   {:name "get_source_block"
    :description "取得單筆素材的完整內容：節錄的原文 text，或表格的欄名 columns 與各列 rows；另有名稱、來源、位置、用途說明、狀態 status（confirmed 可引用、discarded 已捨棄）、素材上的事實 facts（key、label、value、unit、指定的 row 與 column），以及引用它的段落 cited_by（card_id、名稱、number、所屬章節 position）。block_id 由 search_sources 取得。"
    :inputSchema {:type "object"
                  :properties {:block_id block-id-property}
                  :required ["block_id"]
                  :additionalProperties false}
    :handler get-source-block}
   {:name "list_facts"
    :description "列出報告中的全部事實：key、label、value、unit（原始值）、display（畫面顯示的文字）、所屬素材 block_id 與名稱 block_title、表格素材指定的 row（0 起算）與 column；素材已捨棄的標示 block_discarded。撰寫本文要用 {{fact:key}} 引用事實，新增前先查看是否已有。block_id 選填，只列出某筆素材上的事實。"
    :inputSchema {:type "object"
                  :properties {:block_id (assoc block-id-property :description "只列出這筆素材上的事實（選填，UUID）")}
                  :additionalProperties false}
    :handler list-facts}
   {:name "add_excerpts"
    :description (str "批次新增素材節錄：一次要新增多筆節錄時用這個，不要重複呼叫 add_excerpt。excerpts 陣列最多 " source-ops/max-batch-excerpts " 筆，每項的欄位與規則同 add_excerpt（source、title、text、location、note）。全部合法才寫入，整批記成一個修改組（使用者可一次復原）；任何一筆不合法就整批不寫，錯誤會列出每一筆的問題（excerpts[i]（名稱）：原因），同一批內原文重複也算錯誤。修正後整批重送。" verbatim-rule "回傳的 blocks 依序對應 excerpts。")
    :inputSchema {:type "object"
                  :properties {:excerpts {:type "array" :minItems 1 :maxItems source-ops/max-batch-excerpts
                                          :description "要新增的節錄，每項同 add_excerpt 的參數"
                                          :items {:type "object"
                                                  :properties {:source source-property
                                                               :title {:type "string" :description "素材名稱（必填）"}
                                                               :text {:type "string" :description "節錄的原文（必填，照抄）"}
                                                               :location location-property
                                                               :note note-property}
                                                  :required ["source" "title" "text"]
                                                  :additionalProperties false}}
                               :summary summary-property
                               :group group-property}
                  :required ["excerpts"]
                  :additionalProperties false}
    :handler add-excerpts}
   {:name "add_excerpt"
    :description (str "新增一筆素材節錄：你讀過原始資料（PDF、Excel、會議紀錄等）後，把有用的一段原文存進素材庫，之後撰寫時以 [[cite:block_id]] 或 add_citation 引用。" verbatim-rule "節錄要有名稱 title（一眼看出這段講什麼，例如「計畫目標」）、來源檔名 source 與位置 location。新增的素材立即可以引用，不需確認；使用者發現錯誤會捨棄它或留待辦請你修正。新增前先用 search_sources 查詢，避免重複（同一來源已有相同原文會被拒絕）。表格請改用 add_source_table；重新整理或計算過的內容不是素材，直接寫在本文或用 write_table。數字要綁定的值再用 add_fact 記成事實。")
    :inputSchema {:type "object"
                  :properties {:source source-property
                               :title {:type "string" :description "素材名稱（必填）"}
                               :text {:type "string" :description "節錄的原文（必填，照抄）"}
                               :location location-property
                               :note note-property
                               :summary summary-property
                               :group group-property}
                  :required ["source" "title" "text"]
                  :additionalProperties false}
    :handler add-excerpt}
   {:name "add_source_table"
    :description "新增一張表格素材：把原始資料中的數據表照原樣節錄，columns 是欄名（非空陣列），rows 是各列，每列的儲存格數必須等於欄數，儲存格是字串或數字（數字轉成字串）。照抄原表的欄名、列序與儲存格寫法（含單位、百分比符號、千分位逗號），不要重新整理、排序、合計或自己計算；重新整理或計算過的表不是素材，請用 add_card 建表格卡片再 write_table 填寫。表格中要在本文引用的數字用 add_fact 記成事實，可指定 row（0 起算）與 column。新增前先用 search_sources 查詢，避免重複。"
    :inputSchema {:type "object"
                  :properties {:source source-property
                               :title {:type "string" :description "素材名稱（必填），例如「各場域量測結果」"}
                               :columns {:type "array" :items {:type "string"} :description "欄名（非空）"}
                               :rows {:type "array" :description "各列；每列是與 columns 等長的儲存格陣列"
                                      :items {:type "array" :items {:type ["string" "number"]}}}
                               :location location-property
                               :note note-property
                               :summary summary-property
                               :group group-property}
                  :required ["source" "title" "columns" "rows"]
                  :additionalProperties false}
    :handler add-source-table}
   {:name "update_source_block"
    :description "修改素材：title、text（節錄的原文）、location、note（location 與 note 傳空字串表示清除）；表格素材可改 columns 與 rows（要一起給，整張表取代）。改的內容仍須照抄原文（用於修正抄錯的地方）。已捨棄的素材不能修改。改了原文或表格後會重新檢查指向它的事實：修改仍會寫入，值在新內容中找不到的事實列在 fact_problems，請接著用 update_fact 修正。要改某筆素材的內容時先用 get_source_block 看目前內容。"
    :inputSchema {:type "object"
                  :properties {:block_id block-id-property
                               :title {:type "string" :description "新的素材名稱"}
                               :text {:type "string" :description "新的原文（只用於節錄，取代原本的內容）"}
                               :columns {:type "array" :items {:type "string"} :description "新的欄名（只用於表格，要與 rows 一起給）"}
                               :rows {:type "array" :items {:type "array" :items {:type ["string" "number"]}} :description "新的各列（只用於表格，要與 columns 一起給）"}
                               :location (assoc location-property :description "新的位置；空字串清除")
                               :note (assoc note-property :description "新的用途說明；空字串清除")
                               :summary summary-property
                               :group group-property}
                  :required ["block_id"]
                  :additionalProperties false}
    :handler update-source-block}
   {:name "discard_source_block"
    :description "捨棄素材（內容錯誤、重複或不用了）：捨棄後搜尋不到、不能再被引用（write_prose 的 [[cite:...]] 與 add_citation 都會拒絕）；已經引用它的段落保留依據並標示「已捨棄」，回傳的 cited_cards 列出這些段落，請確認本文是否要改寫或改引用其他素材。這是可復原的修改，使用者可在 Knot 畫面復原；沒有恢復工具。"
    :inputSchema {:type "object"
                  :properties {:block_id block-id-property
                               :summary summary-property
                               :group group-property}
                  :required ["block_id"]
                  :additionalProperties false}
    :handler discard-source-block}
   {:name "add_fact"
    :description "新增事實：本文要寫到的數字或關鍵值（站數、人次、比率、金額、日期等），指向它所屬的素材 block_id。value 必須出現在該素材的原文（或指定的儲存格）中——Knot 以 NFKC 正規化並忽略空白後比對，不符時拒絕並說明；照抄素材中的寫法（原文是「2,871」就寫 \"2,871\"）。值與單位分開記錄時 value 只放數字（例如 value \"78.4\"、unit \"%\"），只比對 value。自己算出來的數字（平均、達成率）不是事實，直接寫在本文，會列為「未綁定的數字」。key 由英文小寫字母、數字、「.」、「-」組成（例如 ridership.2025-11、saving-rate.site-a），報告內唯一；label 是顯示用名稱。素材是表格時可用 row（0 起算的列索引）與 column（欄名）指定儲存格，兩者要同時給，不給則比對整張表。寫本文時用 {{fact:key}} 引用，畫面顯示值與單位（四位數以上的整數加千分位、數字與中文單位間空一格，儲存的值不變）。比對時忽略數字的千分位（「1,210」與「1210」視為相同）。新增前先用 list_facts 查看是否已有。"
    :inputSchema {:type "object"
                  :properties {:key {:type "string" :description "事實 key（必填）：小寫英文字母、數字、「.」、「-」，報告內唯一"}
                               :label {:type "string" :description "顯示用名稱（必填），例如「節電率·A」"}
                               :value {:type ["string" "number"] :description "值（必填），照抄素材中的寫法；建議用字串"}
                               :unit {:type "string" :description "單位（選填），例如「%」「kWh」；不屬於 value"}
                               :block_id (assoc block-id-property :description "事實所屬的素材 id（必填，不能是已捨棄的）")
                               :row {:type "integer" :minimum 0 :description "表格素材：列索引（0 起算，選填，要與 column 同時給）"}
                               :column {:type "string" :description "表格素材：欄名（選填，要與 row 同時給）"}
                               :summary summary-property
                               :group group-property}
                  :required ["key" "label" "value" "block_id"]
                  :additionalProperties false}
    :handler add-fact}
   {:name "add_facts"
    :description (str "批次新增事實：一次要新增多筆事實時用這個，不要重複呼叫 add_fact。facts 陣列最多 " source-ops/max-batch-facts " 筆，每項的欄位與檢查同 add_fact（key、label、value、unit、block_id、row、column；value 必須出現在素材中）。全部合法才寫入，整批記成一個修改組（使用者可一次復原）；任何一筆不合法就整批不寫，錯誤會列出每一筆的問題（facts[i]（key）：原因），同一批內 key 重複也算錯誤。修正後整批重送。回傳的 facts 依序對應 facts。")
    :inputSchema {:type "object"
                  :properties {:facts {:type "array" :minItems 1 :maxItems source-ops/max-batch-facts
                                       :description "要新增的事實，每項同 add_fact 的參數"
                                       :items {:type "object"
                                               :properties {:key {:type "string" :description "事實 key（必填）：小寫英文字母、數字、「.」、「-」，報告內唯一"}
                                                            :label {:type "string" :description "顯示用名稱（必填）"}
                                                            :value {:type ["string" "number"] :description "值（必填），照抄素材中的寫法；建議用字串"}
                                                            :unit {:type "string" :description "單位（選填）"}
                                                            :block_id (assoc block-id-property :description "事實所屬的素材 id（必填，不能是已捨棄的）")
                                                            :row {:type "integer" :minimum 0 :description "表格素材：列索引（0 起算，選填，要與 column 同時給）"}
                                                            :column {:type "string" :description "表格素材：欄名（選填，要與 row 同時給）"}}
                                               :required ["key" "label" "value" "block_id"]
                                               :additionalProperties false}}
                               :summary summary-property
                               :group group-property}
                  :required ["facts"]
                  :additionalProperties false}
    :handler add-facts}
   {:name "update_fact"
    :description "修改事實的 label、value、unit（空字串清除）、block_id，或表格儲存格的 row 與 column（要同時給；兩者傳 null 表示改為不指定儲存格）；key 不能改。檢查同 add_fact：新的 value 必須出現在素材中。本文中的 {{fact:key}} 會顯示新值，不必改本文；回傳的 cited_cards 是本文用到這個事實的段落，數字有變時請確認前後文是否仍然通順。"
    :inputSchema {:type "object"
                  :properties {:key {:type "string" :description "要修改的事實 key（由 list_facts 取得）"}
                               :label {:type "string" :description "新的顯示名稱"}
                               :value {:type ["string" "number"] :description "新的值，照抄素材中的寫法"}
                               :unit {:type "string" :description "新的單位；空字串清除"}
                               :block_id (assoc block-id-property :description "改指向另一筆素材（不能是已捨棄的）")
                               :row {:type ["integer" "null"] :minimum 0 :description "表格素材：列索引（0 起算）；null 表示不指定儲存格"}
                               :column {:type ["string" "null"] :description "表格素材：欄名；null 表示不指定儲存格"}
                               :summary summary-property
                               :group group-property}
                  :required ["key"]
                  :additionalProperties false}
    :handler update-fact}
   {:name "delete_fact"
    :description "刪除事實（例如登記錯的事實）。本文還用到 {{fact:key}} 的段落時拒絕並列出段落，請先用 write_prose 改寫，再刪除。刪除記成修改組，使用者可以復原。"
    :inputSchema {:type "object"
                  :properties {:key {:type "string" :description "要刪除的事實 key（由 list_facts 取得）"}
                               :summary summary-property
                               :group group-property}
                  :required ["key"]
                  :additionalProperties false}
    :handler delete-fact}
   {:name "add_citation"
    :description "把素材加入段落或引言的依據（只限段落與引言）；寫本文時用 [[cite:block_id]] 也會自動加入。where 選填，說明引用的範圍（例如「欄：場域、節電率」）。已捨棄的素材不能加；已經是依據時回覆「已經是依據」，不寫入。"
    :inputSchema {:type "object"
                  :properties {:card_id uuid-property
                               :block_id block-id-property
                               :where {:type "string" :description "引用的範圍說明（選填）"}
                               :summary summary-property
                               :group group-property}
                  :required ["card_id" "block_id"]
                  :additionalProperties false}
    :handler add-citation}
   {:name "remove_citation"
    :description "從段落或引言的依據移除素材（含已捨棄的素材）；本文中的 [[cite:...]] 標記不會被移除，要一併處理請用 write_prose。不是依據時回錯誤。現有的依據可用 get_card 查看。"
    :inputSchema {:type "object"
                  :properties {:card_id uuid-property
                               :block_id block-id-property
                               :summary summary-property
                               :group group-property}
                  :required ["card_id" "block_id"]
                  :additionalProperties false}
    :handler remove-citation}])

(def tools (into (into read-tools write-tools) source-tools))


(def ^:private tools-by-name (into {} (map (juxt :name identity)) tools))

;; ---------------------------------------------------------------------------
;; JSON-RPC

(def supported-versions ["2025-11-25" "2025-06-18" "2025-03-26"])

(def instructions
  "Knot 是撰寫結構化報告的本機工具。報告是一棵卡片樹：標題（heading，最多四層，編號由位置決定）、引言（intro，每個標題至多一張）、段落（paragraph，要撰寫本文的單位）、表格（table）與圖（figure）。每張卡片有意圖（要寫什麼）、狀態（none：尚無本文、ai：agent 撰寫、human：使用者改過）與選填的本文。卡片以 card_id 識別、留言以 note_id 識別，都是 UUID 字串；指定要操作或查詢的卡片一律用參數 card_id、留言一律用 note_id。卡片名稱（title）不含編號：編號由位置決定、隨移動重新計算，只出現在 number 欄位，不要把編號寫進名稱。請先呼叫 get_outline 了解整體結構，再用 get_card 看單張卡片（連續看多張用 get_cards；要整份大綱的完整意圖時 get_outline 傳 full_intent: true），撰寫某段前用 get_writing_context 取得應參考的內容（周邊卡片只帶意圖，has_prose 為 true 的表示對方已有本文；需要銜接或用到其結果時用 get_card 取本文），寫好後用 write_prose 交回：本文每一行一段，「- 」開頭是項目清單、「1. 」開頭是編號清單，引用用 {{fact:key}}、{{ref:label}}、[[cite:區塊id]]，不存在的標記會被拒絕。表格與圖先用 add_card 建在段落底下（取得 label），本文再以 {{ref:label}} 引用，表格內容用 write_table 填寫。規劃結構與整理關係可用 add_card、build_outline、update_card、move_card、delete_card、set_relation、add_remote_relation、remove_relation；報告的名稱與副標用 update_report。卡片可設定目標字數（target_chars）；get_outline 與 get_card 回傳每張卡片目前的字數 chars（標題為整章／整節合計，空白不算），get_writing_context 的本段會帶目標字數，撰寫時參考目標字數控制篇幅（大約即可，不必精確）。素材與事實：原始資料（PDF、Excel、會議紀錄等）由你自己讀取，挑出要引用的部分存進素材庫。流程：讀原始資料 → search_sources 看已經有哪些素材（避免重複）→ add_excerpt（一段原文，照抄、不改字不改數字、一段只講一件事）或 add_source_table（數據表照原樣）→ add_fact（要在本文寫到的數字；值必須出現在素材中，表格可指定 row 與 column）→ 撰寫時以 {{fact:key}} 引用事實、[[cite:block_id]] 引用素材（也可用 add_citation）。素材以 block_id 識別（UUID）；重新整理或自己計算的表與數字不是素材與事實，直接寫在本文或用 write_table。一次新增多筆節錄或事實時用批次工具 add_excerpts、add_facts（全部合法才寫入，整批一個修改組）。素材與事實的寫入工具有 add_excerpt、add_excerpts、add_source_table、update_source_block、discard_source_block、add_fact、add_facts、update_fact、delete_fact，讀取用 search_sources、get_source_block、list_facts；依據用 add_citation、remove_citation。使用者可以捨棄素材，已捨棄的素材搜尋不到、不能再引用。使用者會在卡片上留言與交代待辦：開始工作時先用 list_todos 查看。處理待辦或修改卡片前先用 get_card 看 history：使用者改過的內容不要蓋掉，有疑問就用 add_note 留言詢問，而不是自行猜測。完成待辦一律用 complete_todo 並附 reply 說明做了什麼。使用者回覆你的提問後，處理完要用 add_note（reply_to 指向那則提問）在同一串回一句說明你怎麼處理；沒有接話的討論串會一直出現在 list_todos 的 questions_answered。一次工作有多次寫入時，每次寫入都傳相同的 group（例如「依審查意見調整大綱」），Knot 的歷程會顯示為一筆，使用者可以一次復原。所有修改都會記錄，使用者可以在 Knot 畫面上檢視與復原。")

(defn- result [id res] {:jsonrpc "2.0" :id id :result res})
(defn- rpc-error [id code msg] {:jsonrpc "2.0" :id id :error {:code code :message msg}})

(def ^:private renamed-args
  "舊參數名稱 → 新名稱（只列有改名的工具）；傳了舊名稱時回清楚的錯誤，而不是「缺少參數」。"
  (let [card-tools ["get_card" "get_writing_context" "update_card" "move_card" "delete_card"
                    "write_prose" "write_table"]]
    (merge (into {} (for [t card-tools] [t {"id" "card_id"}]))
           {"add_note" {"node_id" "card_id"}
            "list_todos" {"node_id" "card_id"}
            "complete_todo" {"id" "note_id"}})))

(defn- check-renamed-args!
  [tool-name args]
  (doseq [[old new] (get renamed-args tool-name)
          :when (contains? args old)]
    (tool-error! (str "參數 " old " 已改名為 " new "：請改傳 " new "（指卡片一律用 card_id、指留言一律用 note_id）"))))

(defn- call-tool [conn {:strs [arguments] tool-name "name"}]
  (if-let [tool (get tools-by-name tool-name)]
    (try
      (let [args (or arguments {})
            _ (check-renamed-args! tool-name args)
            res ((:handler tool) conn args)]
        {:content [{:type "text" :text (write-json res)}] :structuredContent res})
      (catch clojure.lang.ExceptionInfo e
        (if (::tool-error (ex-data e))
          {:content [{:type "text" :text (ex-message e)}] :isError true}
          (throw e))))
    ::unknown-tool))

(defn- dispatch
  "處理一個 JSON-RPC 請求（有 id），回傳回應 map。"
  [conn {:strs [id method params]}]
  (case method
    "initialize"
    (let [asked (get params "protocolVersion")]
      (result id {:protocolVersion (if (some #{asked} supported-versions) asked "2025-06-18")
                  :capabilities {:tools {}}
                  :serverInfo {:name "knot" :version "0.1"}
                  :instructions instructions}))

    "ping" (result id {})

    "tools/list"
    (result id {:tools (mapv #(select-keys % [:name :description :inputSchema]) tools)})

    "tools/call"
    (let [res (call-tool conn params)]
      (if (= ::unknown-tool res)
        (rpc-error id -32602 (str "未知的工具：" (get params "name")))
        (result id res)))

    (rpc-error id -32601 (str "未知的方法：" method))))

(defn- json-response [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (write-json body)})

(defn handle-rpc
  "處理 POST /mcp 的 body 字串，回傳 ring 回應。"
  [conn body]
  (let [msg (try (read-json body) (catch Exception _ ::parse-error))]
    (cond
      (= ::parse-error msg) (json-response 200 (rpc-error nil -32700 "JSON 解析失敗"))
      (not (and (map? msg) (= "2.0" (get msg "jsonrpc"))))
      (json-response 200 (rpc-error nil -32600 "請求格式不正確（不支援 batch，需為 JSON-RPC 2.0 物件）"))
      ;; 通知與用戶端回覆：沒有 id 或沒有 method，不回內容
      (or (not (contains? msg "id")) (not (string? (get msg "method"))))
      (if (and (contains? msg "id") (not (contains? msg "result")) (not (contains? msg "error")))
        (json-response 200 (rpc-error (get msg "id") -32600 "缺少 method"))
        {:status 202})
      :else (json-response 200 (dispatch conn msg)))))

;; ---------------------------------------------------------------------------
;; HTTP

(defn- local-origin?
  "沒有 Origin 標頭（非瀏覽器的用戶端）或其 host 為本機時通過，防止 DNS rebinding。"
  [origin]
  (or (nil? origin)
      (let [host (try (.getHost (java.net.URI. origin)) (catch Exception _ nil))]
        (contains? #{"localhost" "127.0.0.1" "[::1]"} host))))

(defn handler
  "/mcp 的 ring handler。"
  [conn]
  (fn [req]
    (cond
      (not (local-origin? (get-in req [:headers "origin"])))
      {:status 403 :headers {"Content-Type" "text/plain; charset=utf-8"} :body "Origin 不允許"}

      (not= :post (:request-method req))
      {:status 405 :headers {"Allow" "POST"} :body "此端點只接受 POST"}

      :else (handle-rpc conn (some-> (:body req) (slurp :encoding "UTF-8") (or ""))))))
