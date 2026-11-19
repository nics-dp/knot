(ns knot.mcp-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [jsonista.core :as json]
            [knot.db :as db]
            [knot.notes :as notes]
            [knot.seed :as seed]
            [knot.server :as server]
            [knot.tree :as tree]
            [knot.ui :as ui]))

(def ^:dynamic *handler* nil)
(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [t]
    (let [dir (str (java.nio.file.Files/createTempDirectory "knot-mcp" (make-array java.nio.file.attribute.FileAttribute 0)))
          conn (db/open-report dir)]
      (try
        (seed/seed! conn (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))
        (binding [*handler* (server/handler conn) *conn* conn] (t))
        (finally (db/close-report conn))))))

(defn- post
  "送出原始字串（或資料）到 /mcp，回傳 {:status :body}，body 已解析為 JSON（無 body 為 nil）。"
  [body & [headers]]
  (let [res (*handler* {:request-method :post :uri "/mcp"
                        :headers (merge {"content-type" "application/json"} headers)
                        :body (java.io.ByteArrayInputStream.
                               (.getBytes ^String (if (string? body) body (json/write-value-as-string body)) "UTF-8"))})]
    (assoc res :body (let [b (some-> (:body res) str not-empty)] (try (some-> b json/read-value) (catch Exception _ b))))))

(defn- rpc [method & [params]]
  (post {"jsonrpc" "2.0" "id" 1 "method" method "params" params}))

(defn- call [tool args]
  (:body (rpc "tools/call" {"name" tool "arguments" args})))

(defn- outline [] (get-in (call "get_outline" {}) ["result" "structuredContent"]))

(defn- display
  "測試用的顯示名稱：有編號時為「1.2 名稱」（title 本身不含編號）。"
  [card]
  (if-let [n (get card "number")] (str n " " (get card "title")) (get card "title")))

(defn- card-id [title]
  (some #(when (or (= title (get % "title")) (= title (display %))) (get % "card_id")) (get (outline) "cards")))

(defn- structured [res] (get-in res ["result" "structuredContent"]))

(deftest initialize-test
  (let [res (rpc "initialize" {"protocolVersion" "2025-03-26"})
        r (get-in res [:body "result"])]
    (is (= 200 (:status res)))
    (is (= "application/json" (get-in res [:headers "Content-Type"])))
    (is (= "2025-03-26" (get r "protocolVersion")))
    (is (= {"tools" {}} (get r "capabilities")))
    (is (= "knot" (get-in r ["serverInfo" "name"])))
    (is (string? (get r "instructions"))))
  (is (= "2025-06-18" (get-in (rpc "initialize" {"protocolVersion" "1999-01-01"}) [:body "result" "protocolVersion"]))))

(deftest notification-and-ping-test
  (let [res (post {"jsonrpc" "2.0" "method" "notifications/initialized"})]
    (is (= 202 (:status res)))
    (is (nil? (:body res))))
  (is (= {} (get-in (rpc "ping") [:body "result"]))))

(deftest tools-list-test
  (let [tools (get-in (rpc "tools/list") [:body "result" "tools"])]
    (is (= ["add_card" "add_citation" "add_excerpt" "add_excerpts" "add_fact" "add_facts" "add_note" "add_remote_relation"
            "add_source_table" "build_outline" "complete_todo" "delete_card" "delete_fact" "discard_source_block"
            "get_card" "get_cards" "get_outline" "get_source_block" "get_style" "get_writing_context" "get_writing_order" "list_facts"
            "list_todos" "move_card" "remove_citation" "remove_relation" "search_sources" "set_relation"
            "update_card" "update_fact" "update_report" "update_source_block" "update_style" "write_prose" "write_table"]
           (sort (map #(get % "name") tools))))
    (is (every? #(= "object" (get-in % ["inputSchema" "type"])) tools))
    (is (every? #(seq (get % "description")) tools))))

(deftest get-outline-test
  (let [{:strs [report cards]} (outline)
        by-title (into {} (map (juxt display identity)) cards)]
    (is (= "OO 市智慧路燈示範計畫" (get report "title")))
    (is (= 47 (count cards)))
    (testing "標題的編號與深度"
      (is (= {"kind" "heading" "depth" 1 "number" "第一章" "parent_id" nil}
             (select-keys (by-title "第一章 計畫概述") ["kind" "depth" "number" "parent_id"])))
      (is (= {"depth" 2 "number" "1.1"} (select-keys (by-title "1.1 計畫緣起") ["depth" "number"]))))
    (testing "段落"
      (let [p (by-title "政策背景")]
        (is (= "paragraph" (get p "kind")))
        (is (= "ai" (get p "status")))
        (is (true? (get p "has_prose")))
        (is (nil? (get p "number")))
        (is (= (get (by-title "1.1 計畫緣起") "card_id") (get p "parent_id")))))
    (testing "引言"
      (is (= "intro" (get (by-title "引言") "kind"))))
    (testing "依文件順序：章在其下卡片之前"
      (is (< (.indexOf (mapv #(get % "title") cards) "第一章 計畫概述")
             (.indexOf (mapv #(get % "title") cards) "政策背景"))))))

(deftest get-card-history-test
  (let [id (card-id "主要成果")]
    (testing "匯入後只有一筆"
      (is (= [{"kind" "import" "by" "user" "fields" [] "undone" false}]
             (mapv #(select-keys % ["kind" "by" "fields" "undone"])
                   (get (structured (call "get_card" {"card_id" id})) "history")))))
    (testing "agent 改名稱與意圖：附 text_changes，不含本文"
      (call "update_card" {"card_id" id "title" "改過的標題" "intent" "改過的意圖"})
      (let [h (get (structured (call "get_card" {"card_id" id})) "history")
            e (last h)]
        (is (= 2 (count h)))
        (is (= "agent" (get e "by")))
        (is (= "change" (get e "kind")))
        (is (= ["intent" "name"] (get e "fields")))
        (is (= false (get e "undone")))
        (is (string? (get e "at")))
        (is (= "主要成果" (get-in e ["text_changes" "name" "before"])))
        (is (= "改過的意圖" (get-in e ["text_changes" "intent" "after"])))
        (is (not (contains? e "prose")))))))

(deftest get-card-test
  (let [id (card-id "主要成果")
        c (structured (call "get_card" {"card_id" id}))]
    (is (= id (get c "card_id")))
    (is (= "paragraph" (get c "kind")))
    (is (= "主要成果" (get c "title")))
    (is (vector? (get c "path")))
    (is (every? #(and (contains? % "number") (string? (get % "title"))) (get c "path")))
    (is (string? (get c "intent")))
    (is (= 1 (get c "version")))
    (is (contains? (get c "relations") "prev"))
    (is (vector? (get-in c ["relations" "outgoing"]))))
  (testing "段落的前後相鄰關係與不相鄰關係"
    (let [c (structured (call "get_card" {"card_id" (card-id "各場域結果比較")}))
          rel #(select-keys % ["type" "title" "pending"])]
      (is (= {"type" "遞進" "title" "測試方法概述" "pending" false} (rel (get-in c ["relations" "prev"]))))
      (is (= {"type" "因果" "title" "異常狀況說明" "pending" false} (rel (get-in c ["relations" "next"]))))
      (is (= #{"節能效益估算" "主要成果"} (set (map #(get % "title") (get-in c ["relations" "incoming"])))) "摘要本段的段落")
      (is (= ["摘要" "摘要"] (map #(get % "type") (get-in c ["relations" "incoming"]))))
      (is (= ["3.2" "4.1"] (sort (map #(get % "position") (get-in c ["relations" "incoming"])))))
      (is (= [] (get-in c ["relations" "outgoing"])))))
  (testing "有本文與依據的段落"
    (let [c (structured (call "get_card" {"card_id" (card-id "政策背景")}))]
      (is (= [{"number" "第一章" "title" "計畫概述"} {"number" "1.1" "title" "計畫緣起"}] (get c "path")) "title 不含編號")
      (is (re-find #"節能減碳" (get c "prose")))
      (is (= 1 (count (get c "citations"))))
      (is (= {"type" "因果" "pending" false}
             (select-keys (get-in c ["relations" "next"]) ["type" "pending"])))
      (is (= "現況問題" (get-in c ["relations" "next" "title"])))
      (is (nil? (get-in c ["relations" "prev"])))))
  (testing "標題：下層卡片"
    (let [c (structured (call "get_card" {"card_id" (card-id "1.1 計畫緣起")}))]
      (is (= ["政策背景" "現況問題"] (map #(get % "title") (get c "children"))))
      (is (nil? (get c "prose")))))
  (testing "找不到或格式錯誤"
    (doseq [args [{"card_id" (str (random-uuid))} {"card_id" "xyz"} {}]]
      (let [r (get (call "get_card" args) "result")]
        (is (true? (get r "isError")))
        (is (string? (get-in r ["content" 0 "text"])))))))

(deftest get-writing-context-test
  (let [id (card-id "政策背景")
        res (call "get_writing_context" {"card_id" id})
        r (get res "result")
        ctx (get r "structuredContent")]
    (is (= id (get ctx "card_id")))
    (is (pos? (get ctx "context_chars")))
    (is (not (contains? ctx "chars")) "參考內容的長度叫 context_chars，不與本文字數混淆")
    (is (= (get ctx "context_chars")
           (reduce + (for [s (get ctx "sections") e (get s "entries")]
                       (+ (count (get e "label")) (count (get e "text")))))))
    (is (every? #{"upper" "self" "neighbors" "children" "relations" "sources" "style"}
                (map #(get % "group") (get ctx "sections"))))
    (is (some #(= "self" (get % "group")) (get ctx "sections")))
    (is (some #(= "intent" (get-in % ["entries" 0 "source"])) (get ctx "sections")))
    (is (= ctx (json/read-value (get-in r ["content" 0 "text"])))
        "text 與 structuredContent 為同一份結果"))
  (testing "標題不會撰寫本文"
    (let [r (get (call "get_writing_context" {"card_id" (card-id "第一章 計畫概述")}) "result")]
      (is (true? (get r "isError")))
      (is (= "只有段落與引言會撰寫本文" (get-in r ["content" 0 "text"]))))))

(deftest writing-context-prev-prose-test
  (let [id (card-id "現況問題")
        ctx (structured (call "get_writing_context" {"card_id" id}))
        prev (some #(when (= "前一段" (get % "title")) %) (get ctx "sections"))
        entry (first (get prev "entries"))
        prose (get (structured (call "get_card" {"card_id" (card-id "政策背景")})) "prose")]
    (testing "前一段已有本文：帶本文"
      (is (= "政策背景" (get entry "label")))
      (is (= "prose" (get entry "source")))
      (is (= prose (get entry "text")))
      (is (not (contains? entry "prose_too_long"))))
    (testing "提示用 get_card 取本文"
      (is (re-find #"get_card" (get ctx "note"))))))

(deftest get-writing-order-test
  (let [res (structured (call "get_writing_order" {}))
        batches (get res "batches")
        ids (mapcat #(map (fn [c] (get c "card_id")) %) batches)]
    (is (seq batches))
    (is (= [] (get res "cycle")))
    (is (= (count ids) (count (set ids))) "每張卡片只出現一次")
    (is (= #{"paragraph" "intro"} (set (map #(get % "kind") (apply concat batches)))))
    (is (every? #(= #{"card_id" "number" "title" "kind" "section" "has_prose"} (set (keys %))) (apply concat batches)))
    (testing "標明所屬標題的編號與名稱"
      (let [c (some #(when (= (card-id "政策背景") (get % "card_id")) %) (apply concat batches))]
        (is (= "1.1 計畫緣起" (get c "section")))))
    (testing "前一段排在本段之前"
      (let [pos (into {} (apply concat (map-indexed (fn [i b] (for [c b] [(get c "card_id") i])) batches)))]
        (is (< (pos (card-id "政策背景")) (pos (card-id "現況問題")))))))
  (testing "限定標題"
    (let [h (card-id "1.1 計畫緣起")
          res (structured (call "get_writing_order" {"card_id" h}))
          titles (set (map #(get % "title") (apply concat (get res "batches"))))]
      (is (contains? titles "政策背景"))
      (is (not (contains? titles "引言")) "第一章的引言在範圍外，1.1 沒有引言")
      (is (< (count (apply concat (get res "batches")))
             (count (apply concat (get (structured (call "get_writing_order" {})) "batches")))))))
  (testing "card_id 不是標題"
    (is (true? (get-in (call "get_writing_order" {"card_id" (card-id "政策背景")}) ["result" "isError"])))))

(deftest errors-test
  (is (= -32602 (get-in (call "no_such_tool" {}) ["error" "code"])))
  (is (= -32601 (get-in (rpc "no/such") [:body "error" "code"])))
  (is (= -32700 (get-in (post "{壞掉") [:body "error" "code"])))
  (is (= -32600 (get-in (post "[]") [:body "error" "code"])))
  (is (= -32600 (get-in (post {"id" 1 "method" "ping"}) [:body "error" "code"]))))

(deftest origin-test
  (let [ping {"jsonrpc" "2.0" "id" 1 "method" "ping"}]
    (is (= 403 (:status (post ping {"origin" "http://evil.example"}))))
    (is (= 403 (:status (post ping {"origin" "null"}))))
    (is (= 200 (:status (post ping {"origin" "http://localhost:3000"}))))
    (is (= 200 (:status (post ping {"origin" "http://127.0.0.1:3000"}))))
    (is (= 200 (:status (post ping {"origin" "http://[::1]:3000"}))))
    (is (= 200 (:status (post ping))))))

(deftest get-not-allowed-test
  (is (= 405 (:status (*handler* {:request-method :get :uri "/mcp"})))))

;; ---------------------------------------------------------------------------
;; 寫入工具

(defn- state [] (ui/from-server (db/load-state (d/db *conn*))))

(defn- changesets
  "所有修改組，舊到新：[{:changeset/by :changeset/label :changeset/origin}]。"
  []
  (let [db (d/db *conn*)]
    (->> (d/q '[:find ?id ?at :where [?c :changeset/id ?id] [?c :changeset/at ?at]] db)
         (sort-by second)
         (mapv #(d/pull db [:changeset/id :changeset/by :changeset/label :changeset/origin :changeset/group]
                        [:changeset/id (first %)])))))

(defn- node-by [pred] (some #(when (pred %) %) (vals (:nodes (state)))))
(defn- node-id [title] (:node/id (node-by #(or (= title (:node/title %)) (= title (:node/name %))))))
(defn- by-name [name] (node-by #(= name (:node/name %))))

(defn- error-text [res]
  (let [r (get res "result")]
    (is (true? (get r "isError")))
    (get-in r ["content" 0 "text"])))

(deftest renamed-args-test
  (let [id (card-id "政策背景")]
    (testing "舊名稱 id、node_id 回清楚的錯誤"
      (doseq [tool ["get_card" "get_writing_context" "update_card" "move_card" "delete_card" "write_prose" "write_table"]]
        (let [msg (error-text (call tool {"id" id}))]
          (is (re-find #"已改名為 card_id" msg) tool)))
      (is (re-find #"已改名為 card_id" (error-text (call "add_note" {"node_id" id "text" "x"}))))
      (is (re-find #"已改名為 card_id" (error-text (call "list_todos" {"node_id" id}))))
      (is (re-find #"已改名為 note_id" (error-text (call "complete_todo" {"id" id})))))
    (testing "回傳中指卡片的欄位都是 card_id，沒有 id"
      (let [c (structured (call "get_card" {"card_id" id}))]
        (is (not (contains? c "id")))
        (is (every? #(and (contains? % "card_id") (not (contains? % "id"))) (get-in c ["relations" "outgoing"])))
        (is (every? #(and (contains? % "card_id") (not (contains? % "id"))) (get (outline) "cards")))))))

(deftest add-card-tool-test
  (let [before (count (changesets))
        parent (card-id "1.2 計畫目標")
        para-id (fn [t] (str (:node/id (node-by #(= t (:node/title %))))))
        res (call "add_card" {"kind" "paragraph" "parent_id" parent "after_id" (para-id "計畫目標")
                              "title" "補充說明" "intent" "說明預期效益" "summary" "規劃：補充段落"})
        out (structured res)
        card (first (get out "cards"))
        n (get (:nodes (state)) (parse-uuid (get card "card_id")))]
    (testing "回傳"
      (is (string? (get out "changeset_id")))
      (is (= 1 (count (get out "cards"))))
      (is (= {"kind" "paragraph" "number" nil "title" "補充說明"} (dissoc card "card_id")))
      (is (string? (get out "message"))))
    (testing "資料庫"
      (is (= {:node/title "補充說明" :node/draft "說明預期效益" :node/status :none :node/parent (parse-uuid parent)}
             (select-keys n [:node/title :node/draft :node/status :node/parent])))
      (is (= ["計畫目標" "補充說明"] (map #(:node/title (get (:nodes (state)) %))
                                         (tree/child-ids (:nodes (state)) (parse-uuid parent))))))
    (testing "修改組：agent、label 為 summary"
      (is (= (inc before) (count (changesets))))
      (is (= {:changeset/by :agent :changeset/label "規劃：補充段落" :changeset/origin :manual}
             (dissoc (peek (changesets)) :changeset/id))))
    (testing "復原整組"
      (is (= "規劃：補充段落" (db/undo! *conn*)))
      (is (nil? (node-id "補充說明"))))))

(deftest add-card-tool-default-label-and-chapter-test
  (let [out (structured (call "add_card" {"kind" "heading" "parent_id" nil "title" "附錄"}))]
    (is (= {"kind" "heading" "number" "第五章" "title" "附錄"} (dissoc (first (get out "cards")) "card_id")))
    (is (= "新增標題「附錄」" (:changeset/label (peek (changesets)))))
    (is (= :agent (:changeset/by (peek (changesets)))))))

(deftest add-card-tool-mixed-test
  (let [h (card-id "1.1 計畫緣起")
        kids #(mapv (fn [id] (:node/title (get (:nodes (state)) id) (:node/name (get (:nodes (state)) id))))
                    (tree/child-ids (:nodes (state)) (parse-uuid h)))
        before (kids)
        out (structured (call "add_card" {"kind" "heading" "parent_id" h "title" "新下層"}))
        sub (get (first (get out "cards")) "card_id")]
    (testing "已有段落的標題底下新增下層標題：接在所有段落之後"
      (is (= "1.1.1" (get (first (get out "cards")) "number")))
      (is (= (conj before "新下層") (kids))))
    (testing "再新增段落：放在最後一個段落之後、下層標題之前"
      (call "add_card" {"kind" "paragraph" "parent_id" h "title" "新段落"})
      (is (= (conj before "新段落" "新下層") (kids))))
    (testing "移動段落到下層標題之後被拒絕，into 則放進下層標題"
      (let [n (count (changesets))]
        (is (re-find #"段落要排在下層標題之前"
                     (error-text (call "move_card" {"card_id" (card-id "政策背景") "target_id" sub "position" "after"}))))
        (is (= n (count (changesets)))))
      (call "move_card" {"card_id" (card-id "政策背景") "target_id" sub "position" "into"})
      (is (= (parse-uuid sub) (tree/parent (:nodes (state)) (parse-uuid (card-id "政策背景"))))))
    (testing "build_outline 混放：順序對成功、錯誤時指出項目"
      (is (re-find #"^items\[0\]\.children\[1\]："
                   (error-text (call "build_outline"
                                     {"parent_id" nil
                                      "items" [{"kind" "heading" "title" "混放"
                                                "children" [{"kind" "heading" "title" "子"}
                                                            {"kind" "paragraph" "title" "段"}]}]}))))
      (is (some? (structured (call "build_outline"
                                   {"parent_id" nil
                                    "items" [{"kind" "heading" "title" "混放"
                                              "children" [{"kind" "paragraph" "title" "段"}
                                                          {"kind" "heading" "title" "子"}]}]})))))))

(deftest add-card-tool-errors-test
  (let [before (count (changesets))]
    (testing "規則：段落不能排在下層標題之後、下層標題不能排在段落之前"
      (is (re-find #"段落要排在下層標題之前.*into"
                   (error-text (call "add_card" {"kind" "paragraph" "parent_id" (card-id "第一章 計畫概述")
                                                 "after_id" (card-id "1.1 計畫緣起") "title" "x"}))))
      (is (re-find #"下層標題要排在這一層所有段落之後"
                   (error-text (call "add_card" {"kind" "heading" "parent_id" (card-id "1.1 計畫緣起")
                                                 "after_id" (card-id "政策背景") "title" "x"})))))
    (testing "after_id 不在同層"
      (is (re-find #"after_id 不是 parent_id 的子卡片"
                   (error-text (call "add_card" {"kind" "paragraph" "parent_id" (card-id "1.1 計畫緣起")
                                                 "after_id" (card-id "計畫目標") "title" "x"})))))
    (testing "參數"
      (is (re-find #"kind 只接受" (error-text (call "add_card" {"kind" "bogus" "parent_id" nil "title" "x"}))))
      (is (re-find #"最上層只能放標題" (error-text (call "add_card" {"kind" "table" "parent_id" nil "title" "x"}))))
      (is (re-find #"parent_id" (error-text (call "add_card" {"kind" "heading" "title" "x"}))))
      (is (re-find #"UUID" (error-text (call "add_card" {"kind" "heading" "parent_id" "zzz" "title" "x"})))))
    (is (= before (count (changesets))) "失敗都不寫入")))

(deftest build-outline-tool-test
  (let [before (count (changesets))
        n-before (count (:nodes (state)))
        res (call "build_outline"
                  {"parent_id" nil
                   "summary" "規劃第五章"
                   "items" [{"kind" "heading" "title" "後續規劃" "intent" "說明後續工作"
                             "children" [{"kind" "intro"}
                                         {"kind" "heading" "title" "短期"
                                          "children" [{"kind" "paragraph" "title" "第一年工作" "intent" "…"}]}
                                         {"kind" "heading" "title" "長期"}]}]})
        out (structured res)]
    (testing "依文件順序回傳所有新卡片與編號"
      (is (= [["heading" "第五章"] ["intro" nil] ["heading" "5.1"] ["paragraph" nil] ["heading" "5.2"]]
             (mapv #(vec (map % ["kind" "number"])) (get out "cards"))))
      (is (= ["第五章" "後續規劃"] ((juxt #(get % "number") #(get % "title")) (get-in out ["cards" 0])))))
    (testing "整批一個修改組"
      (is (= (inc before) (count (changesets))))
      (is (= {:changeset/by :agent :changeset/label "規劃第五章" :changeset/origin :manual}
             (dissoc (peek (changesets)) :changeset/id)))
      (is (= (+ n-before 5) (count (:nodes (state))))))
    (testing "一次復原整批"
      (db/undo! *conn*)
      (is (= n-before (count (:nodes (state))))))))

(deftest build-outline-tool-error-test
  (let [before (count (changesets))
        n-before (count (:nodes (state)))
        text (error-text (call "build_outline"
                               {"parent_id" nil
                                "items" [{"kind" "heading" "title" "好的"}
                                         {"kind" "heading" "title" "壞的"
                                          "children" [{"kind" "heading" "title" "子"}
                                                      {"kind" "paragraph" "title" "p"}]}]}))]
    (is (re-find #"^items\[1\]\.children\[1\]：" text))
    (is (= before (count (changesets))))
    (is (= n-before (count (:nodes (state)))) "整批都沒寫"))
  (is (re-find #"items" (error-text (call "build_outline" {"parent_id" nil "items" []})))))

(deftest update-card-tool-test
  (let [id (card-id "選定原則")
        before (count (changesets))]
    (testing "版本不符：拒絕，不寫入"
      (is (= "卡片已被修改（目前版本 1），請先 get_card 重新讀取"
             (error-text (call "update_card" {"card_id" id "title" "新標題" "version" 7}))))
      (is (= before (count (changesets)))))
    (testing "版本相符：寫入，回傳寫入後的卡片"
      (let [out (structured (call "update_card" {"card_id" id "title" "新原則" "intent" "新意圖" "version" 1
                                                  "summary" "改段落名稱"}))]
        (is (= [{"card_id" id "kind" "paragraph" "number" nil "title" "新原則"}] (get out "cards")))
        (is (= {:node/title "新原則" :node/draft "新意圖" :node/version 2}
               (select-keys (get (:nodes (state)) (parse-uuid id)) [:node/title :node/draft :node/version])))
        (is (= {:changeset/by :agent :changeset/label "改段落名稱"}
               (select-keys (peek (changesets)) [:changeset/by :changeset/label])))))
    (testing "標題寫名稱"
      (let [hid (card-id "2.1 場域選定")
            out (structured (call "update_card" {"card_id" hid "title" "場域評估"}))]
        (is (= ["2.1" "場域評估"] ((juxt #(get % "number") #(get % "title")) (get-in out ["cards" 0]))))
        (is (= "場域評估" (:node/name (by-name "場域評估"))))))
    (testing "沒有變動"
      (let [n (count (changesets))
            out (structured (call "update_card" {"card_id" id "title" "新原則"}))]
        (is (nil? (get out "changeset_id")))
        (is (= [] (get out "cards")))
        (is (= n (count (changesets))))))
    (testing "引言不能設定 title"
      (is (re-find #"引言沒有名稱" (error-text (call "update_card" {"card_id" (card-id "引言") "title" "x"})))))))

(deftest move-card-tool-test
  (let [id (card-id "政策背景")
        res (call "move_card" {"card_id" id "target_id" (card-id "現況問題") "position" "after"})
        out (structured res)
        s (state)]
    (is (= ["現況問題" "政策背景"]
           (map #(:node/title (get (:nodes s) %))
                (tree/child-ids (:nodes s) (tree/parent (:nodes s) (parse-uuid id))))))
    (is (re-find #"待確認" (get out "message")))
    (is (= {:changeset/by :agent :changeset/origin :move :changeset/label "移動「政策背景」"}
           (dissoc (peek (changesets)) :changeset/id)))
    (is (some :relation/pending (:relations s)) "相鄰關係標為待確認")
    (testing "不合法"
      (let [n (count (changesets))]
        (is (re-find #"段落要排在下層標題之前"
                     (error-text (call "move_card" {"card_id" id "target_id" (card-id "1.1 計畫緣起") "position" "after"}))))
        (is (re-find #"position" (error-text (call "move_card" {"card_id" id "target_id" id "position" "x"}))))
        (is (= n (count (changesets))))))
    (db/undo! *conn*)
    (is (= ["政策背景" "現況問題"]
           (map #(:node/title (get (:nodes (state)) %))
                (tree/child-ids (:nodes (state)) (parse-uuid (card-id "1.1 計畫緣起"))))))))

(defn- relation [from to]
  (some #(when (and (= (parse-uuid from) (:relation/from %)) (= (parse-uuid to) (:relation/to %))) %)
        (:relations (state))))

(deftest set-relation-tool-test
  (let [a (card-id "選定原則") b (card-id "三處場域簡介")]
    (let [out (structured (call "set_relation" {"from_id" a "to_id" b "type" "causal" "summary" "確認關係"}))]
      (is (= ["選定原則" "三處場域簡介"] (map #(get % "title") (get out "cards"))))
      (is (= {:relation/type :causal :relation/pending false}
             (select-keys (relation a b) [:relation/type :relation/pending])))
      (is (= {:changeset/by :agent :changeset/label "確認關係"}
             (select-keys (peek (changesets)) [:changeset/by :changeset/label]))))
    (testing "不相鄰"
      (let [n (count (changesets))]
        (is (re-find #"不相鄰" (error-text (call "set_relation" {"from_id" a "to_id" (card-id "判定方式") "type" "causal"}))))
        (is (re-find #"type" (error-text (call "set_relation" {"from_id" a "to_id" b "type" "basis"}))))
        (is (= n (count (changesets))))))))

(deftest add-remote-relation-tool-test
  (let [from (card-id "待改進事項") to (card-id "異常斷線處理")
        args {"from_id" from "to_id" to "type" "basis"}
        out (structured (call "add_remote_relation" args))]
    (is (= 2 (count (get out "cards"))))
    (is (= {:relation/type :basis :relation/remote true}
           (select-keys (relation from to) [:relation/type :relation/remote])))
    (is (= :agent (:changeset/by (peek (changesets)))))
    (testing "已存在：不寫入，說明原因"
      (let [n (count (changesets))
            again (structured (call "add_remote_relation" args))]
        (is (nil? (get again "changeset_id")))
        (is (re-find #"已經存在" (get again "message")))
        (is (= n (count (changesets))))))
    (testing "錯誤"
      (is (re-find #"同一張" (error-text (call "add_remote_relation" (assoc args "to_id" from)))))
      (is (re-find #"兩個段落" (error-text (call "add_remote_relation" (assoc args "to_id" (card-id "1.1 計畫緣起"))))))
      (is (re-find #"type" (error-text (call "add_remote_relation" (assoc args "type" "causal"))))))))

(deftest remove-relation-tool-test
  (let [a (card-id "測試方法概述") b (card-id "各場域結果比較")]
    (is (some? (relation a b)))
    (testing "類型不符：不寫入"
      (let [n (count (changesets))]
        (is (re-find #"沒有類型為 causal 的關係"
                     (error-text (call "remove_relation" {"from_id" a "to_id" b "type" "causal"}))))
        (is (= n (count (changesets))))))
    (structured (call "remove_relation" {"from_id" a "to_id" b}))
    (is (nil? (relation a b)))
    (is (= :agent (:changeset/by (peek (changesets)))))
    (testing "不相鄰的關係也能移除"
      (let [from (card-id "節能效益估算") ; 摘要：p321 → p312
            _ (is (= :summarizes (:relation/type (relation from b))))]
        (structured (call "remove_relation" {"from_id" from "to_id" b "type" "summarizes"}))
        (is (nil? (relation from b)))))))

(deftest delete-card-tool-test
  (let [id (card-id "各場域結果比較")
        prev (card-id "測試方法概述")
        next (card-id "異常狀況說明")
        n (count (:nodes (state)))]
    (testing "版本不同：不刪除"
      (is (re-find #"已被修改" (error-text (call "delete_card" {"card_id" id "version" 999}))))
      (is (= n (count (:nodes (state))))))
    (let [out (structured (call "delete_card" {"card_id" id "summary" "刪掉比較段"}))]
      (is (re-find #"待確認" (get out "message")))
      (is (= ["異常狀況說明"] (map #(get % "title") (get out "cards")))))
    (is (nil? (get (:nodes (state)) (parse-uuid id))))
    (is (not-any? #(#{(parse-uuid id)} ((juxt :relation/from :relation/to) %)) (:relations (state))))
    (is (:relation/pending (relation prev next)) "後一段改為承接前一段，待確認")
    (is (= {:changeset/by :agent :changeset/label "刪掉比較段"}
           (select-keys (peek (changesets)) [:changeset/by :changeset/label :changeset/group])))
    (testing "刪除標題連同下層"
      (let [h (card-id "1.1 計畫緣起")
            before (count (:nodes (state)))
            out (structured (call "delete_card" {"card_id" h}))]
        (is (re-find #"張下層卡片" (get out "message")))
        (is (< (count (:nodes (state))) (dec before)))))))

(deftest write-conflict-test
  (with-redefs [db/apply-changes! (fn [_ _] {:conflicts [{:expected 1 :actual 2}]})]
    (is (= "卡片剛被修改，請重新讀取後再試"
           (error-text (call "update_card" {"card_id" (card-id "選定原則") "title" "x"}))))))

;; ---------------------------------------------------------------------------
;; 留言與待辦

(defn- user-note! [m] (notes/add-note! *conn* (merge {:author :user} m)))
(defn- cs-count [] (count (changesets)))

(deftest add-note-tool-test
  (let [c (card-id "計畫目標")
        before (cs-count)
        out (structured (call "add_note" {"card_id" c "text" "目標的數字來源是？"}))
        note (first (notes/list-notes (d/db *conn*)))]
    (is (= (str (:note/id note)) (get out "note_id")))
    (is (string? (get out "message")))
    (is (= {:note/author :agent :note/node (parse-uuid c) :note/text "目標的數字來源是？"}
           (select-keys note [:note/author :note/node :note/text])))
    (is (= before (cs-count)) "不產生修改組")
    (testing "回覆沿用卡片；card_id 可為 null"
      (let [r (structured (call "add_note" {"card_id" nil "text" "補充" "reply_to" (get out "note_id")}))
            reply (last (notes/list-notes (d/db *conn*)))]
        (is (= (str (:note/id reply)) (get r "note_id")))
        (is (= (parse-uuid c) (:note/node reply)))))
    (testing "整份報告"
      (is (nil? (:note/node (notes/add-note! *conn* {:author :agent :text "x"})))))
    (testing "錯誤"
      (is (re-find #"card_id" (error-text (call "add_note" {"text" "x"}))))
      (is (re-find #"空白" (error-text (call "add_note" {"card_id" c "text" " "}))))
      (is (re-find #"找不到卡片" (error-text (call "add_note" {"card_id" (str (random-uuid)) "text" "x"}))))
      (is (re-find #"找不到要回覆" (error-text (call "add_note" {"card_id" c "text" "x" "reply_to" (str (random-uuid))})))))))

(deftest list-todos-tool-test
  (let [c (card-id "計畫目標")
        t (user-note! {:node (parse-uuid c) :text "請寫得更具體" :todo true})
        _ (user-note! {:node nil :text "整份報告的待辦" :todo true})
        _ (user-note! {:node (parse-uuid c) :text "只是留言"})
        _ (notes/add-note! *conn* {:author :agent :node (parse-uuid c) :text "你要我寫幾年？" :reply-to (:note/id t)})
        q (notes/add-note! *conn* {:author :agent :node (parse-uuid c) :text "預算是哪一年？"})
        q2 (notes/add-note! *conn* {:author :agent :node nil :text "整體風格？"})
        _ (user-note! {:reply-to (:note/id q) :text "民國 114 年"})
        _ (notes/add-note! *conn* {:author :agent :reply-to (:note/id q2) :text "收到"})
        _ (user-note! {:reply-to (:note/id q2) :text "再補充"})
        out (structured (call "list_todos" {}))]
    (testing "一份 items：使用者已回答的提問 → 使用者的待辦（依時間）；沒人回覆的提問不列"
      (is (= ["answered_question" "answered_question" "user_todo" "user_todo"] (map #(get % "kind") (get out "items"))))
      (is (= ["預算是哪一年？" "整體風格？" "請寫得更具體" "整份報告的待辦"] (map #(get % "text") (get out "items")))))
    (testing "待辦"
      (let [todo (nth (get out "items") 2)]
        (is (= (str (:note/id t)) (get todo "note_id")))
        (is (= {"card_id" c "title" "計畫目標"} (select-keys (get todo "card") ["card_id" "title"])))
        (is (= [["agent" "你要我寫幾年？"]] (map (juxt #(get % "author") #(get % "text")) (get todo "replies"))))
        (is (string? (get todo "at")))
        (is (re-find #"complete_todo" (get todo "next_action"))))
      (is (nil? (get-in out ["items" 3 "card"]))))
    (testing "使用者已回答的提問"
      (is (= "民國 114 年" (get-in out ["items" 0 "replies" 0 "text"])))
      (is (re-find #"add_note" (get-in out ["items" 0 "next_action"]))))
    (testing "依卡片過濾"
      (let [o (structured (call "list_todos" {"card_id" c}))]
        (is (= ["answered_question" "user_todo"] (map #(get % "kind") (get o "items")))))
      (is (re-find #"找不到卡片" (error-text (call "list_todos" {"card_id" (str (random-uuid))})))))
    (testing "agent 接話後不再列為已回答"
      (notes/add-note! *conn* {:author :agent :reply-to (:note/id q) :text "好的"})
      (is (= ["整體風格？" "請寫得更具體" "整份報告的待辦"] (map #(get % "text") (get (structured (call "list_todos" {})) "items")))))))

(deftest complete-todo-tool-test
  (let [c (card-id "計畫目標")
        t (user-note! {:node (parse-uuid c) :text "請寫得更具體" :todo true})
        id (str (:note/id t))]
    (Thread/sleep 5)
    (let [upd (structured (call "update_card" {"card_id" c "intent" "更具體的意圖"}))
          before (cs-count)
          out (structured (call "complete_todo" {"note_id" id "reply" "已改寫意圖"}))
          done (some #(when (= (:note/id t) (:note/id %)) %) (notes/list-notes (d/db *conn*)))]
      (testing "自動連結最近的 agent 修改組"
        (is (= (get upd "changeset_id") (get out "changeset_id")))
        (is (= (parse-uuid (get upd "changeset_id")) (:note/changeset done)))
        (is (= :done (:note/state done)))
        (is (inst? (:note/done-at done))))
      (testing "回覆"
        (let [r (last (notes/list-notes (d/db *conn*)))]
          (is (= {:note/author :agent :note/text "已改寫意圖" :note/reply-to (:note/id t)}
                 (select-keys r [:note/author :note/text :note/reply-to])))))
      (is (= before (cs-count)) "不產生修改組")
      (is (re-find #"已經完成" (error-text (call "complete_todo" {"note_id" id})))))
    (testing "明確指定修改組、沒有修改時為 nil"
      (let [t2 (user-note! {:node (parse-uuid (card-id "1.1 計畫緣起")) :text "另一件" :todo true})
            cs (str (random-uuid))
            out (structured (call "complete_todo" {"note_id" (str (:note/id t2)) "changeset_id" cs}))]
        (is (= cs (get out "changeset_id"))))
      (let [t3 (user-note! {:node nil :text "報告層級" :todo true})]
        (is (nil? (get (structured (call "complete_todo" {"note_id" (str (:note/id t3))})) "changeset_id")))))
    (testing "錯誤"
      (let [plain (user-note! {:node (parse-uuid c) :text "留言"})]
        (is (re-find #"不是待辦" (error-text (call "complete_todo" {"note_id" (str (:note/id plain))})))))
      (is (re-find #"找不到留言" (error-text (call "complete_todo" {"note_id" (str (random-uuid))}))))
      (is (re-find #"note_id" (error-text (call "complete_todo" {})))))))

(deftest outline-and-card-notes-test
  (let [c (card-id "計畫目標")
        _ (user-note! {:node (parse-uuid c) :text "待辦一" :todo true})
        t2 (user-note! {:node (parse-uuid c) :text "待辦二" :todo true})
        _ (notes/add-note! *conn* {:author :agent :reply-to (:note/id t2) :text "回覆"})
        cards (get (outline) "cards")]
    (is (= 2 (some #(when (= c (get % "card_id")) (get % "open_todos")) cards)))
    (is (not-any? #(contains? % "open_todos") (remove #(= c (get % "card_id")) cards)) "沒有待辦時省略")
    (let [ns (get (structured (call "get_card" {"card_id" c})) "notes")]
      (is (= [["user" "待辦一" true "open" nil] ["user" "待辦二" true "open" nil]
              ["agent" "回覆" nil nil (str (:note/id t2))]]
             (map #(mapv % ["author" "text" "todo" "state" "reply_to"]) ns)))
      (is (every? #(string? (get % "at")) ns)))
    (is (= [] (get (structured (call "get_card" {"card_id" (card-id "1.1 計畫緣起")})) "notes")))
    (notes/set-state! *conn* (:note/id t2) :done {})
    (is (= 1 (some #(when (= c (get % "card_id")) (get % "open_todos")) (get (outline) "cards"))))))

;; ---------------------------------------------------------------------------
;; 分組、報告資訊、名稱不帶編號、移動訊息（試寫後修正）

(deftest group-param-test
  (let [a (card-id "政策背景") b (card-id "現況問題")
        g {"group" "依審查意見調整大綱"}
        card-history #(get (structured (call "get_card" {"card_id" %})) "history")]
    (call "update_card" (assoc g "card_id" a "intent" "一"))
    (call "update_card" (assoc g "card_id" b "intent" "二" "summary" "改現況"))
    (call "update_card" (assoc g "card_id" a "intent" "三"))
    (let [cs (rest (changesets))
          gids (->> (d/q '[:find ?g :where [_ :changeset/group-id ?g]] (d/db *conn*)))]
      (is (= 3 (count cs)))
      (is (= 1 (count gids)) "同組共用 group-id"))
    (testing "每張卡片歷程只有一筆、label 是組名"
      (is (= ["依審查意見調整大綱"] (map #(get % "label") (rest (card-history a)))))
      (is (= ["依審查意見調整大綱"] (map #(get % "label") (rest (card-history b))))))
    (testing "一次復原整組"
      (is (= "依審查意見調整大綱" (db/undo! *conn*)))
      (is (not= "三" (:node/draft (get (:nodes (state)) (parse-uuid a)))))
      (is (every? #(true? (get % "undone")) (filter #(= "change" (get % "kind")) (card-history a)))))
    (testing "沒有 group 時與以前相同（各自一筆）"
      (call "update_card" {"card_id" a "intent" "甲"})
      (call "update_card" {"card_id" a "intent" "乙"})
      (is (= 2 (count (filter #(= "change" (get % "kind")) (take-last 2 (card-history a)))))))
    (testing "schema 說明 group"
      (let [tools (get-in (rpc "tools/list") [:body "result" "tools"])]
        (is (every? #(get-in % ["inputSchema" "properties" "group"])
                    (filter #(#{"add_card" "build_outline" "update_card" "move_card" "set_relation"
                                "add_remote_relation" "remove_relation" "delete_card"} (get % "name")) tools)))))))

(deftest update-report-tool-test
  (let [n (count (changesets))]
    (is (= {"title" "新名稱" "subtitle" "副標"}
           (get (structured (call "update_report" {"title" "新名稱" "subtitle" "副標"})) "report")))
    (is (= {"title" "新名稱" "subtitle" "副標"} (get (outline) "report")))
    (testing "只改副標、空字串清除"
      (call "update_report" {"subtitle" ""})
      (is (= {"title" "新名稱" "subtitle" nil} (get (outline) "report"))))
    (is (= n (count (changesets))) "不產生修改組")
    (testing "錯誤"
      (is (re-find #"至少" (error-text (call "update_report" {}))))
      (is (re-find #"title" (error-text (call "update_report" {"title" "  "})))))))

(deftest title-without-number-test
  (let [c (get (outline) "cards")
        h (some #(when (= "計畫緣起" (get % "title")) %) c)]
    (is (= "1.1" (get h "number")))
    (is (not-any? #(re-find #"^(第.章|\d+(\.\d+)*) " (get % "title")) c))
    (is (every? #(or (get % "number") (not= "heading" (get % "kind"))) c))
    (let [card (structured (call "get_card" {"card_id" (get h "card_id")}))]
      (is (= ["政策背景" "現況問題"] (map #(get % "title") (get card "children"))))
      (is (= [nil nil] (map #(get % "number") (get card "children")))))
    (let [card (structured (call "get_card" {"card_id" (card-id "各場域結果比較")}))]
      (is (every? #(contains? % "number") (get-in card ["relations" "incoming"]))))))

(deftest move-message-uses-new-number-test
  (let [id (card-id "1.2 計畫目標")
        out (structured (call "move_card" {"card_id" id "target_id" (card-id "1.1 計畫緣起") "position" "before"}))]
    (is (re-find #"已將「1\.1 計畫目標」移到" (get out "message")) "用移動後的編號")
    (is (not (re-find #"1\.2 計畫目標" (get out "message"))))))

(deftest instructions-test
  (let [text (get-in (rpc "initialize" {"protocolVersion" "2025-06-18"}) [:body "result" "instructions"])]
    (is (every? #(str/includes? text %) ["reply_to" "get_card" "history" "complete_todo" "group" "update_report" "編號"
                                         "write_prose" "write_table" "{{ref:label}}" "get_style" "update_style"]))))

;; ---------------------------------------------------------------------------
;; 表格、圖與本文（M6）

(defn- block-id [title] (:block/id (some #(when (= title (:block/title %)) %) (:blocks (state)))))

(defn- agent-text
  "agent 交回的本文 fixture；@@block:標題@@ 換成該區塊的 id。"
  [file]
  (str/replace (slurp (io/resource (str "fixtures/agent/" file)))
               #"@@block:(.+?)@@" (fn [[_ title]] (str (block-id title)))))

(deftest add-card-table-tool-test
  (let [parent (card-id "各場域結果比較")
        out (structured (call "add_card" {"kind" "table" "parent_id" parent "title" "指標達成總表"
                                          "intent" "整理各指標" "label" "achievement"}))
        n (get (:nodes (state)) (parse-uuid (get-in out ["cards" 0 "card_id"])))]
    (is (= {:node/kind :table :node/title "指標達成總表" :node/label "tab:achievement" :node/draft "整理各指標"}
           (select-keys n [:node/kind :node/title :node/label :node/draft])))
    (is (= "tab:achievement" (get (first (filter #(= "指標達成總表" (get % "title")) (get (outline) "cards"))) "label"))
        "get_outline 附上 label")
    (is (= "tab:achievement" (get (structured (call "get_card" {"card_id" (str (:node/id n))})) "label")))
    (testing "label 重複與格式錯誤"
      (call "add_card" {"kind" "figure" "parent_id" parent "title" "同代稱的圖" "label" "achievement"})
      (is (= "fig:achievement" (:node/label (node-by #(= "同代稱的圖" (:node/title %)))))
          "圖的前綴是 fig:，同代稱不算重複")
      (is (re-find #"已被「指標達成總表」使用"
                   (error-text (call "add_card" {"kind" "table" "parent_id" parent "title" "又一張" "label" "achievement"}))))
      (is (re-find #"label 代稱"
                   (error-text (call "add_card" {"kind" "table" "parent_id" parent "title" "x" "label" "Bad Label"}))))
      (is (re-find #"只用於表格與圖"
                   (error-text (call "add_card" {"kind" "paragraph" "parent_id" (card-id "1.1 計畫緣起") "title" "x" "label" "a"})))))
    (testing "只能放在段落底下"
      (is (re-find #"底下不能放表格"
                   (error-text (call "add_card" {"kind" "table" "parent_id" (card-id "1.1 計畫緣起") "title" "x"})))))))

(deftest build-outline-table-tool-test
  (let [out (structured (call "build_outline"
                              {"parent_id" (card-id "1.1 計畫緣起")
                               "items" [{"kind" "paragraph" "title" "新段落"
                                         "children" [{"kind" "table" "title" "表" "label" "tt"}
                                                     {"kind" "figure" "title" "圖"}]}]}))]
    (is (= ["paragraph" "table" "figure"] (map #(get % "kind") (get out "cards"))))
    (is (some? (node-by #(= "tab:tt" (:node/label %)))))
    (is (some? (node-by #(re-matches #"fig:[a-z0-9]{4}" (str (:node/label %))))))))

(deftest write-table-tool-test
  (let [parent (card-id "各場域結果比較")
        tid (get-in (structured (call "add_card" {"kind" "table" "parent_id" parent "title" "總表"})) ["cards" 0 "card_id"])
        before (count (changesets))
        out (structured (call "write_table" {"card_id" tid "columns" ["指標" "數值"]
                                             "rows" [["場域數" 3] ["節電率" "47%"]] "summary" "填總表"}))]
    (is (string? (get out "changeset_id")))
    (is (= {:columns ["指標" "數值"] :rows [["場域數" "3"] ["節電率" "47%"]]}
           (:node/table-content (get (:nodes (state)) (parse-uuid tid)))))
    (is (= :ai (:node/status (get (:nodes (state)) (parse-uuid tid)))))
    (is (= {:changeset/by :agent :changeset/label "填總表" :changeset/origin :manual}
           (dissoc (peek (changesets)) :changeset/id)))
    (is (= (inc before) (count (changesets))))
    (testing "get_card 回 table 與 label；沒有內容的表格省略 table"
      (let [c (structured (call "get_card" {"card_id" tid}))]
        (is (= {"columns" ["指標" "數值"] "rows" [["場域數" "3"] ["節電率" "47%"]]} (get c "table")))
        (is (str/starts-with? (get c "label") "tab:"))))
    (testing "撰寫參考內容帶 label、意圖與表格內容"
      (let [ctx (structured (call "get_writing_context" {"card_id" parent}))
            e (->> (get ctx "sections") (filter #(= "下層表格與圖" (get % "title"))) first (#(get % "entries"))
                   (filter #(str/includes? (get % "label") "總表")) first)]
        (is (= "指標｜數值\n場域數｜3\n節電率｜47%" (get e "text")))
        (is (str/starts-with? (get e "label") "tab:"))))
    (testing "復原"
      (is (= "填總表" (db/undo! *conn*)))
      (is (nil? (:node/table-content (get (:nodes (state)) (parse-uuid tid)))))
      (is (not (contains? (structured (call "get_card" {"card_id" tid})) "table")) "沒有內容時省略 table"))
    (testing "錯誤"
      (is (re-find #"第 1 列有 1 個儲存格" (error-text (call "write_table" {"card_id" tid "columns" ["a" "b"] "rows" [["x"]]}))))
      (is (re-find #"columns" (error-text (call "write_table" {"card_id" tid "columns" [] "rows" []}))))
      (is (re-find #"不是表格" (error-text (call "write_table" {"card_id" parent "columns" ["a"] "rows" []}))))
      (is (re-find #"已被修改" (error-text (call "write_table" {"card_id" tid "columns" ["a"] "rows" [] "version" 99})))))))

(deftest write-prose-tool-test
  (let [id (card-id "各場域結果比較")
        before (count (changesets))
        text (agent-text "write-prose-streetlight.txt")
        out (structured (call "write_prose" {"card_id" id "text" text "summary" "撰寫各場域結果"}))
        n (get (:nodes (state)) (parse-uuid id))
        doc (:node/prose n)]
    (testing "回傳：修改組與未綁定的數字"
      (is (string? (get out "changeset_id")))
      (is (= ["2"] (get out "unbound_numbers")))
      (is (re-find #"沒有綁定事實" (get out "message"))))
    (testing "ProseMirror 節點：事實、ref、清單、cite"
      (is (= ["paragraph" "paragraph" "bulletList" "orderedList"] (map :type (:content doc))))
      (is (= #{"saving-rate.site-a" "saving-rate.site-b" "saving-rate.site-c"}
             (set (keep #(when (= "fact" (:type %)) (get-in % [:attrs :key])) (tree-seq coll? seq doc)))))
      (is (some #(= {:type "ref" :attrs {:label "tab:field-results"}} %) (tree-seq coll? seq doc)))
      (is (= 2 (count (:content (second (drop 1 (:content doc))))))))
    (testing "資料庫：事實引用抽出、狀態轉為 ai（原本是 human）、cite 加入依據"
      (is (= :ai (:node/status n)))
      (is (some #(= (block-id "各場域量測結果") (:block %)) (:node/citations n)))
      (is (= 2 (count (:node/citations n))) "原有的兩筆依據保留且不重複")
      (is (= 3 (count (d/q '[:find [?f ...] :in $ ?id :where [?n :node/id ?id] [?n :node/facts ?f]]
                           (d/db *conn*) (parse-uuid id))))))
    (testing "修改組：agent，label 為 summary"
      (is (= (inc before) (count (changesets))))
      (is (= {:changeset/by :agent :changeset/label "撰寫各場域結果" :changeset/origin :manual}
             (dissoc (peek (changesets)) :changeset/id))))
    (testing "get_card 回傳同樣的格式，修改後可直接交回；往返不變"
      (let [c (structured (call "get_card" {"card_id" id}))
            md (get c "prose")]
        (is (= (str/trim text) md))
        (is (= ["2"] (get c "unbound_numbers")))
        (is (= {} (dissoc (structured (call "write_prose" {"card_id" id "text" md})) "changeset_id" "cards" "message" "unbound_numbers"))
            "內容相同時沒有任何變動")))
    (testing "復原：本文與狀態回到先前"
      (is (= "撰寫各場域結果" (db/undo! *conn*)))
      (is (= :human (:node/status (get (:nodes (state)) (parse-uuid id)))))
      (is (= 1 (count (:content (:node/prose (get (:nodes (state)) (parse-uuid id))))))))))

(deftest write-prose-tool-errors-test
  (let [id (card-id "各場域結果比較")
        before (count (changesets))]
    (testing "不存在的標記一次全部列出，附可用選項"
      (let [msg (error-text (call "write_prose" {"card_id" id "text" "{{fact:nope.a}} {{fact:nope.b}} {{ref:tab:zzz}} [[cite:00000000-0000-0000-0000-000000000000]] {{fact:saving-rate.site-a}}"}))]
        (is (re-find #"nope\.a、nope\.b" msg))
        (is (re-find #"tab:zzz" msg))
        (is (re-find #"素材 id 不存在或已捨棄" msg))
        (is (re-find #"saving-rate\.site-a（節電率·A）" msg) "可用的事實 key")
        (is (re-find #"tab:field-results（各場域量測結果）" msg) "可用的 label")))
    (testing "格式錯誤的標記"
      (let [msg (error-text (call "write_prose" {"card_id" id "text" "{{foo:x}} 與 {{fact:}}"}))]
        (is (re-find #"\{\{foo:x\}\}" msg))
        (is (re-find #"\{\{fact:\}\}" msg))))
    (testing "空白、缺少參數、版本衝突、非段落"
      (is (re-find #"空白" (error-text (call "write_prose" {"card_id" id "text" "  \n "}))))
      (is (re-find #"缺少參數 text" (error-text (call "write_prose" {"card_id" id}))))
      (is (re-find #"已被修改" (error-text (call "write_prose" {"card_id" id "text" "新" "version" 99}))))
      (is (re-find #"沒有本文" (error-text (call "write_prose" {"card_id" (card-id "1.1 計畫緣起") "text" "新"}))))
      (is (re-find #"write_table" (error-text (call "write_prose" {"card_id" (str (:node/id (node-by #(= :table (:node/kind %))))) "text" "新"})))))
    (is (= before (count (changesets))) "錯誤都沒有寫入")))

(deftest write-prose-no-facts-hint-test
  (let [sid (card-id "1.1 計畫緣起")
        pid (card-id "政策背景")]
    ;; 沒有事實的報告：清掉所有事實後再試
    (db/apply-changes! *conn* {:origin :manual :label "清事實"
                               :changes {:facts {:delete (mapv :fact/key (:facts (db/load-state (d/db *conn*))))}}})
    (is (some? sid))
    (is (re-find #"還沒有事實" (error-text (call "write_prose" {"card_id" pid "text" "{{fact:a}}"}))))))

(deftest write-prose-no-facts-no-number-reminder-test
  (let [pid (card-id "政策背景")]
    (db/apply-changes! *conn* {:origin :manual :label "清事實"
                               :changes {:facts {:delete (mapv :fact/key (:facts (db/load-state (d/db *conn*))))}}})
    (let [out (structured (call "write_prose" {"card_id" pid "text" "共 2,871 人次，114 年 3 月"}))]
      (is (= [] (get out "unbound_numbers")))
      (is (not (re-find #"數字" (get out "message")))))
    (is (= [] (get (structured (call "get_card" {"card_id" pid})) "unbound_numbers")))))

(deftest write-prose-intro-test
  (let [intro (:node/id (node-by #(= :intro (:node/kind %))))
        before-status (:node/status (get (:nodes (state)) intro))
        out (structured (call "write_prose" {"card_id" (str intro) "text" "本章概述。\n- 重點一\n- 重點二"}))]
    (is (string? (get out "changeset_id")))
    (is (= before-status (:node/status (get (:nodes (state)) intro))) "引言不改狀態")
    (is (re-find #"的引言" (:changeset/label (peek (changesets)))))))


(deftest char-count-fields-test
  (let [id (card-id "政策背景")
        chapter (card-id "第一章 計畫概述")
        table (some #(when (= "table" (get % "kind")) %) (get (outline) "cards"))]
    (testing "get_outline 與 get_card 有 chars；沒設定目標時沒有 target_chars"
      (let [by-id (into {} (map (juxt #(get % "card_id") identity)) (get (outline) "cards"))
            c (structured (call "get_card" {"card_id" id}))]
        (is (pos? (get-in by-id [id "chars"])))
        (is (= (get-in by-id [id "chars"]) (get c "chars")))
        (is (not (contains? (by-id id) "target_chars")))
        (is (not (contains? c "target_chars")))
        (testing "標題為底下的合計"
          (is (> (get-in by-id [chapter "chars"]) (get-in by-id [id "chars"]))))
        (testing "表格沒有字數"
          (is (some? table))
          (is (not (contains? table "chars"))))))
    (testing "update_card 設定、輸出、清除"
      (is (nil? (get (call "update_card" {"card_id" id "target_chars" 400}) "error")))
      (is (= 400 (get (structured (call "get_card" {"card_id" id})) "target_chars")))
      (is (= 400 (some #(when (= id (get % "card_id")) (get % "target_chars")) (get (outline) "cards"))))
      (testing "get_writing_context 本段帶目標字數"
        (let [ctx (structured (call "get_writing_context" {"card_id" id}))
              e (some #(when (= "目標字數" (get % "label")) %)
                      (mapcat #(get % "entries") (filter #(= "本段" (get % "title")) (get ctx "sections"))))]
          (is (= 400 (get e "target_chars")))
          (is (str/includes? (get e "text") "約 400 字"))))
      (is (nil? (get (call "update_card" {"card_id" id "target_chars" nil}) "error")))
      (is (not (contains? (structured (call "get_card" {"card_id" id})) "target_chars"))
          "傳 null 清除"))
    (testing "不合法的值回錯誤，不寫入"
      (doseq [bad [0 -1 1.5 "400"]]
        (is (re-find #"正整數" (error-text (call "update_card" {"card_id" id "target_chars" bad}))) (pr-str bad)))
      (is (not (contains? (structured (call "get_card" {"card_id" id})) "target_chars"))))
    (testing "add_card 與 build_outline 可帶 target_chars"
      (let [out (structured (call "add_card" {"kind" "paragraph" "parent_id" (card-id "1.2 計畫目標") "title" "新段"
                                              "target_chars" 250}))
            new-id (get-in out ["cards" 0 "card_id"])]
        (is (= 250 (get (structured (call "get_card" {"card_id" new-id})) "target_chars"))))
      (let [out (structured (call "build_outline" {"parent_id" nil
                                                   "items" [{"kind" "heading" "title" "附錄" "target_chars" 2000
                                                             "children" [{"kind" "paragraph" "title" "附一" "target_chars" 300}]}]}))
            [h p] (map #(get % "card_id") (get out "cards"))]
        (is (= [2000 300] [(get (structured (call "get_card" {"card_id" h})) "target_chars")
                           (get (structured (call "get_card" {"card_id" p})) "target_chars")]))))
    (testing "tools/list 的 description 與 instructions 提到目標字數"
      (let [tools (get-in (rpc "tools/list") [:body "result" "tools"])
            props (fn [n] (get-in (some #(when (= n (get % "name")) %) tools) ["inputSchema" "properties"]))]
        (is (every? #(contains? (props %) "target_chars") ["add_card" "update_card"]))
        (is (str/includes? (get-in (rpc "initialize" {}) [:body "result" "instructions"]) "目標字數"))))))

(deftest get-outline-full-intent-test
  (let [id (card-id "政策背景")
        long-intent (apply str (repeat 30 "說明政策"))]
    (call "update_card" {"card_id" id "intent" long-intent})
    (let [intent-of (fn [args] (some #(when (= id (get % "card_id")) (get % "intent"))
                                     (get (structured (call "get_outline" args)) "cards")))]
      (testing "預設截斷為 80 字"
        (is (= (str (subs long-intent 0 80) "…") (intent-of {}))))
      (testing "full_intent 回傳完整意圖"
        (is (= long-intent (intent-of {"full_intent" true}))))
      (testing "格式錯誤"
        (is (re-find #"full_intent" (error-text (call "get_outline" {"full_intent" "yes"}))))))))

(deftest get-cards-test
  (let [a (card-id "政策背景") b (card-id "第一章 計畫概述") missing (str (random-uuid))]
    (testing "依序回傳，內容與 get_card 相同"
      (let [out (structured (call "get_cards" {"card_ids" [a b]}))]
        (is (= [a b] (mapv #(get % "card_id") (get out "cards"))))
        (is (= (structured (call "get_card" {"card_id" a})) (first (get out "cards"))))
        (is (nil? (get out "errors")))))
    (testing "找不到或格式不對的 id 列在 errors，不影響其他"
      (let [out (structured (call "get_cards" {"card_ids" [a missing "xyz"]}))]
        (is (= [a] (mapv #(get % "card_id") (get out "cards"))))
        (is (= [missing "xyz"] (mapv #(get % "card_id") (get out "errors"))))
        (is (every? #(seq (get % "error")) (get out "errors")))))
    (testing "參數錯誤"
      (is (re-find #"card_ids" (error-text (call "get_cards" {"card_ids" []}))))
      (is (re-find #"card_ids" (error-text (call "get_cards" {"card_ids" "x"}))))
      (is (re-find #"最多 30" (error-text (call "get_cards" {"card_ids" (repeat 31 a)})))))))

;; ---------------------------------------------------------------------------
;; 風格（SPEC 5.7）

(defn- style-entry [id]
  (let [ctx (structured (call "get_writing_context" {"card_id" id}))]
    (first (get (some #(when (= "style" (get % "group")) %) (get ctx "sections")) "entries"))))

(deftest style-tools-test
  (let [id (card-id "政策背景")]
    (testing "尚未設定：text 為空字串並提示；撰寫參考提示尚未設定"
      (let [r (structured (call "get_style" {}))]
        (is (= "" (get r "text")))
        (is (= 0 (get r "chars")))
        (is (re-find #"尚未設定" (get r "message"))))
      (is (= "尚未設定風格" (get (style-entry id) "text"))))
    (testing "使用者在畫面輸入（寫入修改組）後，get_style 取得相同內容"
      (db/apply-changes! *conn* {:by :user :label "編輯文字"
                                 :changes {:style {:upsert [{:style/key "report" :style/text "使用「本計畫」"}]}}})
      (let [r (structured (call "get_style" {}))]
        (is (= "使用「本計畫」" (get r "text")))
        (is (= 7 (get r "chars")))
        (is (= "user" (get r "updated_by")))
        (is (string? (get r "updated_at")))))
    (testing "update_style：記成 agent 的修改組、可復原"
      (let [n (count (changesets))
            out (structured (call "update_style" {"text" "一律使用第三人稱。" "group" "整理風格"}))]
        (is (string? (get out "changeset_id")))
        (is (= (inc n) (count (changesets))))
        (is (= {:changeset/by :agent :changeset/label "修改風格" :changeset/group "整理風格"}
               (select-keys (last (changesets)) [:changeset/by :changeset/label :changeset/group])))
        (let [r (structured (call "get_style" {}))]
          (is (= "一律使用第三人稱。" (get r "text")))
          (is (= "agent" (get r "updated_by"))))
        (testing "get_writing_context 只帶提示（字數、最後修改），不含全文"
          (let [t (get (style-entry id) "text")]
            (is (re-find #"共 9 字" t))
            (is (str/includes? t "get_style"))
            (is (str/includes? t "agent"))
            (is (not (str/includes? t "第三人稱")))))
        (is (= "整理風格" (db/undo! *conn*)))
        (is (= "使用「本計畫」" (get (structured (call "get_style" {})) "text")))))
    (testing "相同內容沒有變動；空字串清除"
      (is (nil? (get (structured (call "update_style" {"text" "使用「本計畫」"})) "changeset_id")))
      (is (some? (get (structured (call "update_style" {"text" ""})) "changeset_id")))
      (is (= "" (get (structured (call "get_style" {})) "text")))
      (db/undo! *conn*)
      (is (= "使用「本計畫」" (get (structured (call "get_style" {})) "text"))))
    (testing "缺少 text"
      (is (re-find #"text" (error-text (call "update_style" {})))))))

(deftest agent-write-creates-knot-todos-mcp-test
  (let [id (card-id "各場域結果比較")
        notes-of #(filterv (comp #{:knot} :note/author) (notes/list-notes (d/db *conn*)))]
    (testing "有本文的依賴段落會留下待辦，回應列出 todos_created；還沒有本文的（主要成果、第三章引言）不留"
      (let [out (structured (call "write_prose" {"card_id" id "text" "改寫" "summary" "改寫"}))
            created (get out "todos_created")]
        (is (= #{["節能效益估算" "3.2" "摘要它"] ["異常狀況說明" "3.1" "因果承接"]}
               (set (map (juxt #(get % "title") #(get % "number") #(get % "reason")) created))))
        (is (every? #(string? (get % "card_id")) created))
        (is (re-find #"已在 2 個相關段落留下待辦：.*3\.1 異常狀況說明（因果承接）" (get out "message")))
        (is (re-find #"下一輪" (get out "message")))
        (is (nil? (get out "dependents")))
        (is (= 2 (count (notes-of))))
        (testing "同一觸發卡片已有未完成待辦不重複"
          (is (nil? (get (structured (call "write_prose" {"card_id" id "text" "再改寫"})) "todos_created")))
          (is (= 2 (count (notes-of)))))))
    (testing "依賴的段落都沒有本文時不留待辦"
      (let [out (structured (call "write_prose" {"card_id" (card-id "主要成果") "text" "第一次寫"}))]
        (is (nil? (get out "todos_created")))
        (is (not (re-find #"留下待辦" (get out "message"))))
        (is (= 2 (count (notes-of))))))
    (testing "update_card 改意圖才留；只改名稱不留"
      (doseq [n (notes-of)] (notes/set-state! *conn* (:note/id n) :done {}))
      (is (nil? (get (structured (call "update_card" {"card_id" id "title" "各場域結果"})) "todos_created")))
      (is (seq (get (structured (call "update_card" {"card_id" id "intent" "新意圖"})) "todos_created"))))
    (testing "instructions 提到規則"
      (is (re-find #"下一輪" (get-in (rpc "initialize" {"protocolVersion" "2025-03-26"}) [:body "result" "instructions"]))))))

(deftest knot-todo-mcp-test
  (let [id (card-id "各場域結果比較")
        id-a (card-id "異常狀況說明")
        load #(ui/from-server (db/load-state (d/db *conn*)))
        before (load)
        p311 (parse-uuid (card-id "測試方法概述"))
        ;; p312 與 p311 都改了；異常狀況說明另以依據關係指向 p311（只加在修改後的狀態）
        after (-> before
                  (assoc-in [:nodes (parse-uuid id) :node/draft] "使用者改過")
                  (assoc-in [:nodes p311 :node/prose] {:type "doc" :content [{:type "paragraph"}]})
                  (update :relations conj {:relation/from (parse-uuid id-a) :relation/to p311
                                           :relation/type :basis :relation/remote true}))
        _ (notes/record-change! *conn* before after)
        out (structured (call "list_todos" {}))
        items (get out "items")
        todo (first (filter #(= "異常狀況說明" (get-in % ["card" "title"])) items))]
    (testing "只有 Knot 待辦：依目標段落的文件順序"
      (is (every? #(= "knot_todo" (get % "kind")) items))
      (is (= ["各場域結果比較" "異常狀況說明" "節能效益估算"] (map #(get-in % ["card" "title"]) items))
          "p312 因 p311 遞進承接也有一則；主要成果與引言還沒有本文，不留"))
    (testing "同一目標合併成一則，triggers 列出每個觸發與原因"
      (is (= 1 (count (filter #(= "異常狀況說明" (get-in % ["card" "title"])) items))))
      (is (= [["測試方法概述" "argument" "以它為依據"] ["各場域結果比較" "argument" "因果承接"]]
             (map (juxt #(get % "title") #(get % "dependency") #(get % "reason")) (get todo "triggers"))))
      (is (= {"card_id" id "number" "3.1"} (select-keys (second (get todo "triggers")) ["card_id" "number"])))
      (is (re-find #"改了，本段以它們為依據、因果承接它們" (get todo "text")))
      (is (re-find #"get_card" (get todo "next_action")))
      (is (nil? (get todo "modified_after"))))
    (testing "提醒之後目標段落又被修改：modified_after"
      (Thread/sleep 5)
      (call "update_card" {"card_id" id-a "intent" "改了意圖"})
      (let [todo' (first (filter #(= "異常狀況說明" (get-in % ["card" "title"])) (get (structured (call "list_todos" {})) "items")))]
        (is (true? (get todo' "modified_after")))
                (is (true? (:note/modified-after (first (filter #(= (parse-uuid id-a) (:note/node %)) (notes/list-notes (d/db *conn*)))))))))
    (testing "complete_todo 完成 Knot 待辦，可附回覆"
      (let [n (count (get (structured (call "list_todos" {})) "items"))
            res (call "complete_todo" {"note_id" (get todo "note_id") "reply" "不需要修改"})]
        (is (not (get-in res ["result" "isError"])))
        (is (= (dec n) (count (get (structured (call "list_todos" {})) "items"))))
        (is (= "不需要修改" (:note/text (last (notes/list-notes (d/db *conn*))))))))))

(deftest same-group-auto-complete-todo-test
  (let [id (card-id "各場域結果比較")
        id-a (card-id "異常狀況說明")
        todo-of (fn [card] (first (filter #(and (= :knot (:note/author %)) (= (parse-uuid card) (:note/node %)))
                                          (notes/list-notes (d/db *conn*)))))]
    (testing "同一 group：先改觸發段落，再改目標段落 → 待辦自動完成、連到修改組、回應有 todos_completed"
      (let [out1 (structured (call "write_prose" {"card_id" id "text" "重寫一" "group" "重寫第三章"}))]
        (is (= 2 (count (get out1 "todos_created"))))
        (is (= :open (:note/state (todo-of id-a))))
        (is (every? #(string? (:group-id %)) (:note/triggers (todo-of id-a))) "觸發記下 group-id"))
      (let [out2 (structured (call "write_prose" {"card_id" id-a "text" "跟著重寫" "group" "重寫第三章"}))
            t (todo-of id-a)]
        (is (= [["異常狀況說明" "3.1"]] (map (juxt #(get % "title") #(get % "number")) (get out2 "todos_completed"))))
        (is (every? string? (map #(get % "card_id") (get out2 "todos_completed"))))
        (is (re-find #"已自動完成 1 則同組內已改寫的相關段落待辦" (get out2 "message")))
        (is (= :done (:note/state t)))
        (is (= (parse-uuid (get out2 "changeset_id")) (:note/changeset t)))
        (is (= {:note/author :knot :note/text "同一組內已改寫（重寫第三章）" :note/reply-to (:note/id t)}
               (select-keys (last (notes/list-notes (d/db *conn*))) [:note/author :note/text :note/reply-to])))
        (testing "list_todos 不再列出；其他段落的待辦照舊"
          (let [titles (map #(get-in % ["card" "title"]) (get (structured (call "list_todos" {})) "items"))]
            (is (not (some #{"異常狀況說明"} titles)))
            (is (some #{"節能效益估算"} titles))))))))

(deftest different-group-todo-not-completed-test
  (let [id (card-id "各場域結果比較")
        id-a (card-id "異常狀況說明")]
    (call "write_prose" {"card_id" id "text" "重寫一" "group" "甲組"})
    (testing "不同組：不完成，之後已修改"
      (Thread/sleep 5)
      (let [out (structured (call "write_prose" {"card_id" id-a "text" "另一組改寫" "group" "乙組"}))
            items (get (structured (call "list_todos" {})) "items")
            todo (first (filter #(= "異常狀況說明" (get-in % ["card" "title"])) items))]
        (is (nil? (get out "todos_completed")))
        (is (some? todo))
        (is (true? (get todo "modified_after")))))
    (testing "沒有 group：不完成"
      (call "write_prose" {"card_id" id-a "text" "沒有組"})
      (is (some #(= "異常狀況說明" (get-in % ["card" "title"])) (get (structured (call "list_todos" {})) "items"))))))

(deftest set-relation-across-headings-tool-test
  (let [nodes (:nodes (state))
        pairs (:prev (tree/adjacent-pairs nodes))
        [to from] (first (filter (fn [[to from]] (not= (tree/parent nodes to) (tree/parent nodes from))) pairs))
        out (structured (call "set_relation" {"from_id" (str from) "to_id" (str to) "type" "progression"}))]
    (is (some? to) "範例報告有跨標題相鄰的段落")
    (is (= 2 (count (get out "cards"))))
    (is (= {:relation/type :progression :relation/pending false}
           (select-keys (relation (str from) (str to)) [:relation/type :relation/pending])))
    (testing "不相鄰的錯誤說出實際的前一段"
      (let [far (first (remove #{from to} (keys pairs)))
            msg (error-text (call "set_relation" {"from_id" (str far) "to_id" (str to) "type" "causal"}))]
        (is (re-find #"前一段是" msg))))))

(deftest same-group-intro-auto-complete-and-retrigger-test
  (let [intro (:node/id (node-by #(= :intro (:node/kind %))))
        ;; 該引言所屬標題底下需有本文的段落才會觸發；改寫引言前先確認它有待辦
        todo-of (fn [card] (first (filter #(and (= :knot (:note/author %)) (= card (:note/node %)))
                                          (notes/list-notes (d/db *conn*)))))
        state0 (state)
        h (tree/parent (:nodes state0) intro)
        under (first (filter #(and (tree/paragraph? (:nodes state0) %) (seq (:node/prose (get (:nodes state0) %))))
                             (tree/descendant-ids (:nodes state0) h)))]
    (testing "同一 group 內先改段落（觸發上層引言的待辦）、再改寫引言 → 引言的待辦自動完成"
      (call "write_prose" {"card_id" (str intro) "text" "引言先有本文"})
      (call "write_prose" {"card_id" (str under) "text" "改寫" "group" "整章重寫"})
      (is (= :open (:note/state (todo-of intro))) "引言有待辦")
      (let [out (structured (call "write_prose" {"card_id" (str intro) "text" "引言跟著重寫" "group" "整章重寫"}))]
        (is (= [(str intro)] (map #(get % "card_id") (get out "todos_completed"))))
        (is (= :done (:note/state (todo-of intro))))))
    (testing "重複觸發更新 group-id：同一段落在另一組再改，目標在該組改寫仍自動完成"
      (call "write_prose" {"card_id" (str under) "text" "再改一" "group" "第二組"})
      (is (some #(and (= :knot (:note/author %)) (= intro (:note/node %)) (= :open (:note/state %)))
                (notes/list-notes (d/db *conn*))))
      (let [out (structured (call "write_prose" {"card_id" (str intro) "text" "第二組改寫" "group" "第二組"}))]
        (is (= 1 (count (get out "todos_completed"))))))))

;; ---------------------------------------------------------------------------
;; 附錄

(deftest build-outline-appendix-tool-test
  (let [out (structured
             (call "build_outline"
                   {"parent_id" nil
                    "items" [{"kind" "heading" "title" "後續規劃"}
                             {"kind" "heading" "title" "逐週數據" "appendix" true
                              "children" [{"kind" "paragraph" "title" "週報"
                                           "children" [{"kind" "table" "title" "週次表" "label" "weekly"}
                                                       {"kind" "figure" "title" "趨勢圖"}]}
                                          {"kind" "heading" "title" "細項"}]}
                             {"kind" "heading" "title" "問卷" "appendix" true}]}))
        cards (get out "cards")]
    (testing "回應的編號、appendix 與表格、圖的 label"
      (is (= [["第五章" nil] ["附錄 A" true] [nil nil] [nil nil] [nil nil] ["A.1" nil] ["附錄 B" true]]
             (mapv #(vector (get % "number") (get % "appendix")) cards)))
      (is (= "tab:weekly" (get (nth cards 3) "label")))
      (is (re-matches #"fig:[a-z0-9]{4}" (get (nth cards 4) "label"))))
    (testing "get_outline 與 get_card 帶 appendix"
      (let [o (get (outline) "cards")
            a (some #(when (= "逐週數據" (get % "title")) %) o)]
        (is (= ["附錄 A" true] ((juxt #(get % "number") #(get % "appendix")) a)))
        (is (= "A.1" (get (some #(when (= "細項" (get % "title")) %) o) "number")))
        (is (nil? (get (some #(when (= "計畫概述" (get % "title")) %) o) "appendix")))
        (let [card (structured (call "get_card" {"card_id" (get a "card_id")}))]
          (is (true? (get card "appendix")))
          (is (= "附錄 A" (get card "number"))))))
    (testing "一般章排在附錄章之後：整批不寫，指出 items[i]"
      (let [n (count (changesets))
            text (error-text (call "build_outline"
                                   {"parent_id" nil
                                    "items" [{"kind" "heading" "title" "甲" "appendix" true}
                                             {"kind" "heading" "title" "乙"}]}))]
        (is (re-find #"^items\[1\]：一般章要排在所有附錄章之前" text))
        (is (= n (count (changesets))))))
    (testing "非章帶 appendix"
      (is (re-find #"appendix 只用於章"
                   (error-text (call "add_card" {"kind" "paragraph" "parent_id" (card-id "1.1 計畫緣起") "title" "x" "appendix" true}))))
      (is (re-find #"appendix 只用於章"
                   (error-text (call "build_outline"
                                     {"parent_id" nil
                                      "items" [{"kind" "heading" "title" "甲"
                                                "children" [{"kind" "heading" "title" "乙" "appendix" true}]}]})))))
    (testing "add_card：一般章放在附錄前、after_id 落在錯誤區域"
      (let [r (structured (call "add_card" {"kind" "heading" "parent_id" nil "title" "新一般章"}))]
        (is (= "第六章" (get-in r ["cards" 0 "number"]))))
      (is (re-find #"不能排在附錄之後"
                   (error-text (call "add_card" {"kind" "heading" "parent_id" nil "title" "x" "after_id" (card-id "附錄 A 逐週數據")}))))
      (let [r (structured (call "add_card" {"kind" "heading" "parent_id" nil "title" "附錄新" "appendix" true}))]
        (is (= ["附錄 C" true] ((juxt #(get-in % ["cards" 0 "number"]) #(get-in % ["cards" 0 "appendix"])) r)))))))

(deftest update-card-appendix-tool-test
  (let [id (card-id "第二章 執行方法")
        before (count (changesets))]
    (testing "設為附錄：移到附錄區最後，後面的一般章重新編號"
      (let [out (structured (call "update_card" {"card_id" id "appendix" true}))]
        (is (= ["附錄 A" true] ((juxt #(get-in % ["cards" 0 "number"]) #(get-in % ["cards" 0 "appendix"])) out)))
        (is (= (inc before) (count (changesets))))
        (is (= "第二章" (get (some #(when (= "執行成果" (get % "title")) %) (get (outline) "cards")) "number")))
        (is (= ["第一章" "第二章" "第三章" "附錄 A"]
               (mapv #(get % "number") (filter #(= 1 (get % "depth")) (get (outline) "cards")))))))
    (testing "取消附錄：移到一般章最後"
      (call "update_card" {"card_id" (card-id "第一章 計畫概述") "appendix" true})
      (is (= ["附錄 A" "附錄 B"]
             (mapv #(get % "number") (filter #(and (= 1 (get % "depth")) (get % "appendix")) (get (outline) "cards")))))
      (let [out (structured (call "update_card" {"card_id" id "appendix" false}))]
        (is (= "第三章" (get-in out ["cards" 0 "number"])))
        (is (nil? (get-in out ["cards" 0 "appendix"])))))
    (testing "已是該狀態：不寫入"
      (let [n (count (changesets))
            out (structured (call "update_card" {"card_id" id "appendix" false}))]
        (is (nil? (get out "changeset_id")))
        (is (= n (count (changesets))))))
    (testing "非章"
      (is (re-find #"appendix 只用於章"
                   (error-text (call "update_card" {"card_id" (card-id "選定原則") "appendix" true})))))))

(deftest move-card-appendix-tool-test
  (call "update_card" {"card_id" (card-id "第四章 結論與建議") "appendix" true})
  (let [a (card-id "第一章 計畫概述")
        x (card-id "附錄 A 結論與建議")
        n (count (changesets))]
    (is (re-find #"不能排在附錄之後" (error-text (call "move_card" {"card_id" a "target_id" x "position" "after"}))))
    (is (re-find #"附錄章要排在所有一般章之後" (error-text (call "move_card" {"card_id" x "target_id" a "position" "before"}))))
    (is (= n (count (changesets))))
    (testing "同區可以"
      (is (some? (structured (call "move_card" {"card_id" a "target_id" (card-id "第三章 執行成果") "position" "after"})))))))

(deftest get-outline-last-edited-test
  (let [cards (fn [] (into {} (map (juxt #(get % "title") identity)) (get (outline) "cards")))
        edited (fn [title] ((juxt #(get % "last_edited_by") #(string? (get % "last_edited_at"))) (get (cards) title)))
        id (card-id "選定原則")
        edit-draft! (fn [by text]
                      (let [node (get (:nodes (state)) (parse-uuid id))]
                        (Thread/sleep 5)
                        (db/apply-changes! *conn* {:by by :label "編輯文字" :origin :manual
                                                   :changes {:nodes {:upsert [(assoc node :node/draft text)]}}})))]
    (testing "匯入建立的卡片沒有修改紀錄：省略欄位"
      (is (= [nil false] (edited "選定原則"))))
    (testing "agent 建立的卡片為 agent"
      (call "add_card" {"kind" "paragraph" "parent_id" (card-id "1.1 計畫緣起") "title" "新段落"})
      (is (= ["agent" true] (edited "新段落"))))
    (testing "使用者修改後為 user，其他卡片不受影響"
      (edit-draft! :user "使用者改過")
      (is (= ["user" true] (edited "選定原則")))
      (is (= ["agent" true] (edited "新段落"))))
    (testing "agent 再修改回到 agent"
      (call "update_card" {"card_id" id "intent" "agent 又改"})
      (is (= ["agent" true] (edited "選定原則"))))
    (testing "使用者修改後復原，回到先前未被復原的那一筆"
      (edit-draft! :user "使用者再改")
      (is (= "user" (first (edited "選定原則"))))
      (db/undo! *conn*)
      (is (= "agent" (first (edited "選定原則")))))
    (testing "其他卡片不受使用者修改與復原影響"
      (is (= ["agent" true] (edited "新段落"))))
    (testing "時間是 ISO 字串"
      (is (re-matches #"\d{4}-\d{2}-\d{2}T.*Z" (get (get (cards) "新段落") "last_edited_at"))))
    (testing "description 說明欄位用途"
      (let [d (some #(when (= "get_outline" (get % "name")) (get % "description")) (get-in (rpc "tools/list") [:body "result" "tools"]))]
        (is (str/includes? d "last_edited_by"))
        (is (str/includes? d "get_cards"))))))
