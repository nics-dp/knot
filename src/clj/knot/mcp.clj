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
  "取出參數 id 並確認卡片存在。"
  [{:keys [nodes]} args]
  (let [raw (get args "id")
        id (when (string? raw) (parse-uuid raw))]
    (cond
      (nil? raw) (tool-error! "缺少參數 id")
      (nil? id) (tool-error! (str "id 格式不正確（應為 UUID）：" raw))
      (not (contains? nodes id)) (tool-error! (str "找不到卡片：" raw "。請先用 get_outline 取得有效的 id"))
      :else id)))

(defn- status-name [node] (name (or (:node/status node) :none)))

(defn- card-title
  "顯示用名稱（標題為「第一章 名稱」或「1.2 名稱」，其餘為標題欄位，引言為「引言」）。"
  [nodes numbers id]
  (tree/display-title nodes numbers id))

(defn- inst-str [d] (some-> d .toInstant str))

(defn- truncate [s n]
  (let [s (or s "")]
    (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- has-prose? [node]
  (boolean (some-> (:node/prose node) :content seq)))

;; ---------------------------------------------------------------------------
;; get_outline

(defn- get-outline [conn _args]
  (let [{:keys [nodes report]} (current-state conn)
        numbers (tree/numbering nodes)
        todo-counts (notes/todo-counts (d/db conn))]
    {:report {:title (:report/title report) :subtitle (:report/subtitle report)}
     :cards (vec (for [id (tree/document-order nodes)
                       :let [n (get nodes id)]]
                   (cond-> {:id (str id)
                    :kind (name (:node/kind n))
                    :depth (tree/depth nodes id)
                    :parent_id (some-> (:node/parent n) str)
                    :number (ops/heading-number nodes numbers id)
                    :title (card-title nodes numbers id)
                    :intent (truncate (:node/draft n) 80)
                    :status (status-name n)
                    :has_prose (has-prose? n)
                    :version (:node/version n)}
                     (pos? (get todo-counts id 0)) (assoc :open_todos (get todo-counts id)))))}))

;; ---------------------------------------------------------------------------
;; get_card

(defn- relation-type-label [r]
  (some-> (:relation/type r) context/relation-label))

(defn- neighbor [nodes numbers id rel]
  (when id
    {:id (str id)
     :title (card-title nodes numbers id)
     :type (relation-type-label rel)
     :pending (boolean (:relation/pending rel))}))

(defn- remote-entries [nodes numbers rels other-key]
  (vec (for [r rels
             :let [other (get r other-key)]]
         {:type (relation-type-label r)
          :id (str other)
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
  (let [{:keys [nodes relations blocks facts] :as state} (current-state conn)
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
     {:id (str id)
      :kind (name (:node/kind n))
      :number (ops/heading-number nodes numbers id)
      :title (card-title nodes numbers id)
      :path (vec (for [a (reverse (tree/ancestor-ids nodes id))
                       :when (tree/heading? nodes a)]
                   (card-title nodes numbers a)))
      :parent_id (some-> (:node/parent n) str)
      :intent (or (:node/draft n) "")
      :status (status-name n)
      :version (:node/version n)
      :prose (when (has-prose? n) (context/prose-text facts (:node/prose n)))
      :citations (vec (for [{:keys [block where]} (:node/citations n)
                            :let [b (block-by-id block)]
                            :when b]
                        {:block_id (str block) :block_title (:block/title b) :where where}))
      :relations {:prev (neighbor nodes numbers prev (when prev (tree/relation-between relations prev id)))
                  :next (neighbor nodes numbers nxt (when nxt (tree/relation-between relations id nxt)))
                  :outgoing (remote-entries nodes numbers outgoing :relation/to)
                  :incoming (remote-entries nodes numbers incoming :relation/from)}
      :children (vec (for [k (tree/child-ids nodes id)]
                       {:id (str k)
                        :kind (name (tree/kind nodes k))
                        :title (card-title nodes numbers k)}))
      :history (mapv history-entry (take-last 10 (db/node-history (d/db conn) id)))
      :notes (vec (for [note (notes/list-notes (d/db conn))
                        :when (= id (:note/node note))]
                    (cond-> {:id (str (:note/id note))
                             :author (name (:note/author note))
                             :text (:note/text note)
                             :at (inst-str (:note/at note))}
                      (:note/todo note) (assoc :todo true :state (name (:note/state note)))
                      (:note/reply-to note) (assoc :reply_to (str (:note/reply-to note))))))}
      (:node/label n) (assoc :label (:node/label n)))))

;; ---------------------------------------------------------------------------
;; get_writing_context

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
                                         (:warn e) (assoc :warn true)
                                         (:key e) (assoc :key (:key e)))))}))
     :chars (:chars ctx)}))

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
  {:id (str id)
   :kind (name (:node/kind (get nodes id)))
   :number (ops/heading-number nodes numbers id)
   :title (card-title nodes numbers id)})

(defn- write!
  "執行寫入工具：op 收目前狀態、回傳 knot.ops 的結果。
  錯誤與衝突以 isError 回報；成功時回傳修改組 id 與這次新增或修改的卡片（編號與名稱為寫入後的）。"
  [conn args op]
  (let [old (current-state conn)
        {:keys [error state label message note touched origin]} (op old)
        _ (when error (tool-error! error))
        changes (sync/diff (sync/data old) (sync/data state))]
    (if-not changes
      {:changeset_id nil :cards [] :message (or note "沒有任何變動：內容與目前相同")}
      (let [summary (let [s (get args "summary")] (when-not (str/blank? (when (string? s) s)) s))
            res (db/apply-changes! conn {:changes changes :by :agent :check-versions? true
                                         :origin origin :label (or summary label)})
            nodes (:nodes state)
            numbers (tree/numbering nodes)]
        (when (:conflicts res) (tool-error! "卡片剛被修改，請重新讀取後再試"))
        {:changeset_id (str (:changeset/id res))
         :cards (vec (for [id (distinct touched) :when (contains? nodes id)]
                       (card-summary nodes numbers id)))
         :message (or message (str "已" label "；所有修改都記錄在 Knot，可在畫面上檢視與復原"))}))))

(defn- add-card [conn args]
  (let [kind (enum-arg args "kind" [:heading :paragraph :intro])]
    (when-not (contains? args "parent_id") (tool-error! "缺少參數 parent_id（新增一章時為 null）"))
    (write! conn args
            #(ops/add-card % {:kind kind
                              :parent-id (uuid-arg args "parent_id" false)
                              :after-id (uuid-arg args "after_id" false)
                              :title (get args "title")
                              :intent (get args "intent")}))))

(defn- parse-items
  "build_outline 的 items 轉成 knot.ops 的格式（kind 轉 keyword，其餘原樣；格式問題交給 ops 回報）。"
  [items]
  (if (sequential? items)
    (mapv (fn [item]
            (if (map? item)
              (cond-> {:kind (some-> (get item "kind") (as-> k (when (string? k) (keyword k))))
                       :title (get item "title")
                       :intent (get item "intent")}
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
            #(ops/update-card % {:id (card-id % args)
                                 :title (get args "title")
                                 :intent (get args "intent")
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
        only (when (some? (get args "node_id"))
               (card-id state {"id" (get args "node_id")}))
        wanted? #(or (nil? only) (= only (:note/node %)))]
    {:todos (vec (for [t (notes/open-todos db) :when (wanted? t)]
                   {:id (str (:note/id t))
                    :card (note-card nodes numbers t)
                    :text (:note/text t)
                    :at (inst-str (:note/at t))
                    :replies (reply-entries (notes/thread db (:note/id t)))}))
     :questions_answered
     (vec (for [q (notes/agent-threads db) :when (wanted? q)
                :let [replies (notes/thread db (:note/id q))]
                :when (= :user (:note/author (last replies)))]
            {:id (str (:note/id q))
             :card (note-card nodes numbers q)
             :question (:note/text q)
             :replies (reply-entries replies)}))}))

(defn- add-note [conn args]
  (when-not (contains? args "node_id") (tool-error! "缺少參數 node_id（整份報告的留言為 null）"))
  (let [node (uuid-arg args "node_id" false)
        reply-to (uuid-arg args "reply_to" false)
        res (notes/add-note! conn {:node node :author :agent :text (get args "text") :reply-to reply-to})]
    (when-let [msg (:error res)] (tool-error! msg))
    {:note_id (str (:note/id res))
     :message (if reply-to "已回覆；使用者會在 Knot 畫面上看到" "已留言；使用者會在 Knot 畫面上看到")}))

(defn- complete-todo [conn args]
  (let [id (uuid-arg args "id" true)
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
   :properties {:id {:type "string" :description "卡片 id（UUID，由 get_outline 取得）"}}
   :required ["id"]
   :additionalProperties false})

(def read-tools
  [{:name "get_outline"
    :description "取得整份報告的卡片樹（依文件順序的扁平清單）與報告資訊：id、類型、深度、編號、名稱、意圖摘要、狀態、是否有本文，以及未完成待辦數 open_todos（沒有待辦時省略）。這是規劃與定位卡片的起點；意圖已截斷為前 80 字，完整內容請用 get_card。"
    :inputSchema {:type "object" :properties {} :additionalProperties false}
    :handler get-outline}
   {:name "get_card"
    :description "取得單張卡片的完整資料：完整意圖、本文純文字、所在章節路徑、依據的素材、前後段與其他段落的關係、下層卡片、這張卡片上的留言與待辦（notes），以及最近 10 筆修改紀錄 history（舊到新；每筆有時間、修改者 user 或 agent、修改組名稱、類型、改了哪些欄位、是否已復原，名稱與意圖有變時附 text_changes 的前後文字，不含本文全文）。history 讓你知道使用者改了什麼，例如處理待辦前先看，避免蓋掉使用者剛改的內容。要看某張卡片的細節或確認它的關係時使用；id 由 get_outline 取得。"
    :inputSchema id-schema
    :handler get-card}
   {:name "get_writing_context"
    :description "取得撰寫某段本文時應參考的內容（章節路徑、本段意圖、依據的素材與事實、前後段、有關係的段落、文風），與 Knot 編輯區的「撰寫時參考的內容」相同。撰寫本文時以它為依據；只有段落與引言適用。"
    :inputSchema id-schema
    :handler get-writing-context}
   {:name "list_todos"
    :description "列出使用者交代的待辦與使用者的回覆。todos＝尚未完成的待辦（含所屬卡片，整份報告的待辦 card 為 null，以及討論串中的回覆）；questions_answered＝你先前用 add_note 提問、使用者已回答但你尚未接話的討論串。開始工作時先呼叫；處理完待辦用 complete_todo。node_id 選填，只列出某張卡片的。"
    :inputSchema {:type "object"
                  :properties {:node_id {:type "string" :description "只列出這張卡片的（選填，UUID）"}}
                  :additionalProperties false}
    :handler list-todos}])

(def ^:private summary-property
  {:type "string" :description "這次修改的名稱（選填），會顯示在 Knot 的復原提示；省略時由 Knot 依操作產生"})

(def ^:private uuid-property {:type "string" :description "卡片 id（UUID）"})

(def ^:private outline-item-schema
  {:type "object"
   :properties {:kind {:type "string" :enum ["heading" "paragraph" "intro"]}
                :title {:type "string" :description "標題名稱或段落標題；引言不需要"}
                :intent {:type "string" :description "意圖：這張卡片要寫什麼"}
                :children {:type "array" :description "下層卡片（只有標題能有），格式同本項，可再巢狀"
                           :items {:type "object"}}}
   :required ["kind"]})

(def write-tools
  [{:name "add_card"
    :description "新增一張卡片（標題、段落或引言）。規則同 Knot 畫面：引言每個標題最多一張且在最前面；引言之後的子卡片要嘛全是段落、要嘛全是下層標題，不能混用；標題最多四層。parent_id 為 null 表示新增一章。after_id 指定接在同一層的哪張卡片之後；省略時引言放最前面、其他放最後。新段落的狀態為 none，本文之後再寫。不合法時會回傳具體原因，不會寫入。一次要建立多張卡片請用 build_outline。"
    :inputSchema {:type "object"
                  :properties {:kind {:type "string" :enum ["heading" "paragraph" "intro"]
                                      :description "heading＝標題、paragraph＝段落、intro＝引言"}
                               :parent_id {:type ["string" "null"] :description "上層標題的 id；null 表示新增一章"}
                               :after_id {:type "string" :description "同一層的某張卡片 id，新卡片接在它後面（選填）"}
                               :title {:type "string" :description "標題名稱或段落標題；引言不需要"}
                               :intent {:type "string" :description "意圖：這張卡片要寫什麼（選填）"}
                               :summary summary-property}
                  :required ["kind" "parent_id"]
                  :additionalProperties false}
    :handler add-card}
   {:name "build_outline"
    :description "一次建立巢狀的報告骨架（標題、段落、引言），整批寫成一個修改組，使用者可一次復原。規劃章節結構時使用。任何一處不合法就整批不寫，錯誤會指出是哪一項（例如 items[1].children[0]）與原因。規則與 add_card 相同；只有標題能有 children，同一個標題底下的子卡片要嘛全是段落、要嘛全是下層標題。回傳所有新卡片（依文件順序，含編號與 id），之後可用 id 繼續操作。"
    :inputSchema {:type "object"
                  :properties {:parent_id {:type ["string" "null"] :description "放在哪個標題底下；null 表示放在最上層（建立章）"}
                               :after_id {:type "string" :description "同一層的某張卡片 id，第一項接在它後面（選填，省略時放最後）"}
                               :items {:type "array" :description "要建立的卡片，依序排列" :items outline-item-schema}
                               :summary summary-property}
                  :required ["parent_id" "items"]
                  :additionalProperties false}
    :handler build-outline}
   {:name "update_card"
    :description "修改卡片的名稱或意圖（標題的名稱、段落的標題、卡片的意圖）；不修改本文。傳入 get_card 取得的 version 可避免覆蓋使用者剛做的修改：版本不同時不會寫入。引言沒有名稱。"
    :inputSchema {:type "object"
                  :properties {:id uuid-property
                               :title {:type "string" :description "新的標題名稱（標題）或段落標題（段落）"}
                               :intent {:type "string" :description "新的意圖，會取代原本的內容"}
                               :version {:type "integer" :description "讀取時的卡片版本（選填）；與目前版本不同時拒絕寫入"}
                               :summary summary-property}
                  :required ["id"]
                  :additionalProperties false}
    :handler update-card}
   {:name "move_card"
    :description "移動卡片到另一張卡片的前面、後面或裡面，規則與使用者在畫面拖曳相同（層級、引言位置、最多四層）。移動段落後，受影響的相鄰段落關係會標為「待確認」，請使用者或之後用 set_relation 確認；標題移動後編號會重新計算。不合法時回傳原因。"
    :inputSchema {:type "object"
                  :properties {:id {:type "string" :description "要移動的卡片 id"}
                               :target_id {:type "string" :description "放置位置參考的卡片 id"}
                               :position {:type "string" :enum ["before" "after" "into"]
                                          :description "before＝放在目標之前、after＝放在目標之後、into＝放進目標底下（標題的最後；引言放最前面）"}
                               :summary summary-property}
                  :required ["id" "target_id" "position"]
                  :additionalProperties false}
    :handler move-card}
   {:name "set_relation"
    :description "設定兩個相鄰段落（同一標題下前後相接，from 在前）的關係並確認（取消「待確認」）。不相鄰或不是段落時回錯誤；不相鄰的段落請用 add_remote_relation。"
    :inputSchema {:type "object"
                  :properties {:from_id {:type "string" :description "前一個段落的 id"}
                               :to_id {:type "string" :description "緊接在後的段落 id"}
                               :type {:type "string" :enum ["progression" "causal" "contrast" "example"]
                                      :description "progression＝遞進、causal＝因果、contrast＝對比、example＝舉例"}
                               :summary summary-property}
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
                               :summary summary-property}
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
                               :summary summary-property}
                  :required ["from_id" "to_id"]
                  :additionalProperties false}
    :handler remove-relation}
   {:name "delete_card"
    :description "刪除一張卡片與它所有的下層卡片（例如刪除標題會一併刪除底下的段落），相關的關係也一併移除；原本接在後面的段落改為承接前一段，關係標為「待確認」。刪除會記錄成一個修改組，使用者可在 Knot 畫面復原。刪除前請用 get_card 確認內容；傳入 version 可避免刪掉使用者剛修改的卡片。報告至少要保留一張卡片。"
    :inputSchema {:type "object"
                  :properties {:id uuid-property
                               :version {:type "integer" :description "讀取時的卡片版本（選填）；與目前版本不同時拒絕刪除"}
                               :summary summary-property}
                  :required ["id"]
                  :additionalProperties false}
    :handler delete-card}
   {:name "add_note"
    :description "在卡片上留言給使用者，或回覆討論串。意圖不清楚、素材互相矛盾、需要使用者決定時用它提問，而不是自己猜；也用於回覆使用者的留言。reply_to 給被回覆的留言 id 時，留言會接在該討論串且沿用它的卡片（node_id 可省略但若給了必須相同）；node_id 為 null 表示整份報告的留言。留言不是內容修改，不會產生修改組，也不能建立待辦。之後可用 list_todos 的 questions_answered 看使用者的回答。"
    :inputSchema {:type "object"
                  :properties {:node_id {:type ["string" "null"] :description "留言所屬卡片的 id；null 表示整份報告"}
                               :text {:type "string" :description "留言內容"}
                               :reply_to {:type "string" :description "被回覆的留言 id（選填）"}}
                  :required ["node_id" "text"]
                  :additionalProperties false}
    :handler add-note}
   {:name "complete_todo"
    :description "把使用者的待辦標為完成。changeset_id 選填：省略時 Knot 會連結待辦建立之後、你對該卡片所做的最近一個修改組（沒有則不連結）。reply 選填，會以你的身分回覆在該待辦底下，用來說明做了什麼。不是待辦或已完成時回錯誤。完成待辦本身不是內容修改，不會產生修改組。"
    :inputSchema {:type "object"
                  :properties {:id {:type "string" :description "待辦的留言 id（由 list_todos 取得）"}
                               :reply {:type "string" :description "完成後的說明（選填）"}
                               :changeset_id {:type "string" :description "完成待辦所做的修改組 id（選填，寫入工具回傳的 changeset_id）"}}
                  :required ["id"]
                  :additionalProperties false}
    :handler complete-todo}])

(def tools (into read-tools write-tools))

(def ^:private tools-by-name (into {} (map (juxt :name identity)) tools))

;; ---------------------------------------------------------------------------
;; JSON-RPC

(def supported-versions ["2025-11-25" "2025-06-18" "2025-03-26"])

(def instructions
  "Knot 是撰寫結構化報告的本機工具。報告是一棵卡片樹：標題（heading，最多四層，編號由位置決定）、引言（intro，每個標題至多一張）、段落（paragraph，要撰寫本文的單位）、表格（table）與圖（figure）。每張卡片有意圖（要寫什麼）、狀態（none：尚無本文、ai：agent 撰寫、human：使用者改過）與選填的本文。卡片 id 都是 UUID 字串。請先呼叫 get_outline 了解整體結構，再用 get_card 看單張卡片，撰寫某段前用 get_writing_context 取得應參考的內容。規劃結構與整理關係可用 add_card、build_outline、update_card、move_card、delete_card、set_relation、add_remote_relation、remove_relation。使用者會在卡片上留言與交代待辦：開始工作時先用 list_todos 查看，做完用 complete_todo 完成；不確定使用者的意圖時用 add_note 留言詢問，而不是自行猜測。所有修改都會記錄，使用者可以在 Knot 畫面上檢視與復原。")

(defn- result [id res] {:jsonrpc "2.0" :id id :result res})
(defn- rpc-error [id code msg] {:jsonrpc "2.0" :id id :error {:code code :message msg}})

(defn- call-tool [conn {:strs [arguments] tool-name "name"}]
  (if-let [tool (get tools-by-name tool-name)]
    (try
      (let [res ((:handler tool) conn (or arguments {}))]
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
