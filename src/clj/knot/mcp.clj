(ns knot.mcp
  "MCP 端點（SPEC 5.12）：Streamable HTTP 傳輸、JSON-RPC 2.0，POST /mcp。
  工具以資料登記：{:name :description :inputSchema :handler}，handler 收 (conn args)，
  回傳結果 map（key 為英文 snake_case 的 keyword）；工具層面的錯誤丟 (tool-error! 中文說明)。
  keyword 與 JSON 字串的轉換集中在 `write-json` 與 `read-json`。"
  (:require [datalevin.core :as d]
            [jsonista.core :as json]
            [knot.context :as context]
            [knot.db :as db]
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

(defn- truncate [s n]
  (let [s (or s "")]
    (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- heading-number
  "標題編號：章為「第一章」，其餘為「1.2」；非標題為 nil。"
  [nodes numbers id]
  (when (tree/heading? nodes id)
    (let [num (get numbers id)]
      (if (= 1 (tree/depth nodes id))
        (str "第" (tree/chinese-numeral (parse-long num)) "章")
        num))))

(defn- has-prose? [node]
  (boolean (some-> (:node/prose node) :content seq)))

;; ---------------------------------------------------------------------------
;; get_outline

(defn- get-outline [conn _args]
  (let [{:keys [nodes report]} (current-state conn)
        numbers (tree/numbering nodes)]
    {:report {:title (:report/title report) :subtitle (:report/subtitle report)}
     :cards (vec (for [id (tree/document-order nodes)
                       :let [n (get nodes id)]]
                   {:id (str id)
                    :kind (name (:node/kind n))
                    :depth (tree/depth nodes id)
                    :parent_id (some-> (:node/parent n) str)
                    :number (heading-number nodes numbers id)
                    :title (card-title nodes numbers id)
                    :intent (truncate (:node/draft n) 80)
                    :status (status-name n)
                    :has_prose (has-prose? n)
                    :version (:node/version n)}))}))

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
      :number (heading-number nodes numbers id)
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
                        :title (card-title nodes numbers k)}))}
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
;; 工具登記

(def id-schema
  {:type "object"
   :properties {:id {:type "string" :description "卡片 id（UUID，由 get_outline 取得）"}}
   :required ["id"]
   :additionalProperties false})

(def tools
  [{:name "get_outline"
    :description "取得整份報告的卡片樹（依文件順序的扁平清單）與報告資訊：id、類型、深度、編號、名稱、意圖摘要、狀態、是否有本文。這是規劃與定位卡片的起點；意圖已截斷為前 80 字，完整內容請用 get_card。"
    :inputSchema {:type "object" :properties {} :additionalProperties false}
    :handler get-outline}
   {:name "get_card"
    :description "取得單張卡片的完整資料：完整意圖、本文純文字、所在章節路徑、依據的素材、前後段與其他段落的關係、下層卡片。要看某張卡片的細節或確認它的關係時使用；id 由 get_outline 取得。"
    :inputSchema id-schema
    :handler get-card}
   {:name "get_writing_context"
    :description "取得撰寫某段本文時應參考的內容（章節路徑、本段意圖、依據的素材與事實、前後段、有關係的段落、文風），與 Knot 編輯區的「撰寫時參考的內容」相同。撰寫本文時以它為依據；只有段落與引言適用。"
    :inputSchema id-schema
    :handler get-writing-context}])

(def ^:private tools-by-name (into {} (map (juxt :name identity)) tools))

;; ---------------------------------------------------------------------------
;; JSON-RPC

(def supported-versions ["2025-11-25" "2025-06-18" "2025-03-26"])

(def instructions
  "Knot 是撰寫結構化報告的本機工具。報告是一棵卡片樹：標題（heading，最多四層，編號由位置決定）、引言（intro，每個標題至多一張）、段落（paragraph，要撰寫本文的單位）、表格（table）與圖（figure）。每張卡片有意圖（要寫什麼）、狀態（none、ai、human、locked）與選填的本文。卡片 id 都是 UUID 字串。請先呼叫 get_outline 了解整體結構，再用 get_card 看單張卡片，撰寫某段前用 get_writing_context 取得應參考的內容。")

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
