(ns knot.mcp-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [jsonista.core :as json]
            [knot.db :as db]
            [knot.seed :as seed]
            [knot.server :as server]))

(def ^:dynamic *handler* nil)

(use-fixtures :each
  (fn [t]
    (let [dir (str (java.nio.file.Files/createTempDirectory "knot-mcp" (make-array java.nio.file.attribute.FileAttribute 0)))
          conn (db/open-report dir)]
      (try
        (seed/seed! conn (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))
        (binding [*handler* (server/handler conn)] (t))
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

(defn- card-id [title]
  (some #(when (= title (get % "title")) (get % "id")) (get (outline) "cards")))

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
    (is (= ["get_card" "get_outline" "get_writing_context"] (sort (map #(get % "name") tools))))
    (is (every? #(= "object" (get-in % ["inputSchema" "type"])) tools))
    (is (every? #(seq (get % "description")) tools))))

(deftest get-outline-test
  (let [{:strs [report cards]} (outline)
        by-title (into {} (map (juxt #(get % "title") identity)) cards)]
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
        (is (= (get (by-title "1.1 計畫緣起") "id") (get p "parent_id")))))
    (testing "引言"
      (is (= "intro" (get (by-title "引言") "kind"))))
    (testing "依文件順序：章在其下卡片之前"
      (is (< (.indexOf (mapv #(get % "title") cards) "第一章 計畫概述")
             (.indexOf (mapv #(get % "title") cards) "政策背景"))))))

(deftest get-card-test
  (let [id (card-id "主要成果")
        c (structured (call "get_card" {"id" id}))]
    (is (= id (get c "id")))
    (is (= "paragraph" (get c "kind")))
    (is (= "主要成果" (get c "title")))
    (is (vector? (get c "path")))
    (is (every? string? (get c "path")))
    (is (string? (get c "intent")))
    (is (= 1 (get c "version")))
    (is (contains? (get c "relations") "prev"))
    (is (vector? (get-in c ["relations" "outgoing"]))))
  (testing "段落的前後相鄰關係與不相鄰關係"
    (let [c (structured (call "get_card" {"id" (card-id "各場域結果比較")}))
          rel #(select-keys % ["type" "title" "pending"])]
      (is (= {"type" "遞進" "title" "測試方法概述" "pending" false} (rel (get-in c ["relations" "prev"]))))
      (is (= {"type" "因果" "title" "異常狀況說明" "pending" false} (rel (get-in c ["relations" "next"]))))
      (is (= #{"節能效益估算" "主要成果"} (set (map #(get % "title") (get-in c ["relations" "incoming"])))) "摘要本段的段落")
      (is (= ["摘要" "摘要"] (map #(get % "type") (get-in c ["relations" "incoming"]))))
      (is (= ["3.2" "4.1"] (sort (map #(get % "position") (get-in c ["relations" "incoming"])))))
      (is (= [] (get-in c ["relations" "outgoing"])))))
  (testing "有本文與依據的段落"
    (let [c (structured (call "get_card" {"id" (card-id "政策背景")}))]
      (is (= ["第一章 計畫概述" "1.1 計畫緣起"] (get c "path")))
      (is (re-find #"節能減碳" (get c "prose")))
      (is (= 1 (count (get c "citations"))))
      (is (= {"type" "因果" "pending" false}
             (select-keys (get-in c ["relations" "next"]) ["type" "pending"])))
      (is (= "現況問題" (get-in c ["relations" "next" "title"])))
      (is (nil? (get-in c ["relations" "prev"])))))
  (testing "標題：下層卡片"
    (let [c (structured (call "get_card" {"id" (card-id "1.1 計畫緣起")}))]
      (is (= ["政策背景" "現況問題"] (map #(get % "title") (get c "children"))))
      (is (nil? (get c "prose")))))
  (testing "找不到或格式錯誤"
    (doseq [args [{"id" (str (random-uuid))} {"id" "xyz"} {}]]
      (let [r (get (call "get_card" args) "result")]
        (is (true? (get r "isError")))
        (is (string? (get-in r ["content" 0 "text"])))))))

(deftest get-writing-context-test
  (let [id (card-id "政策背景")
        res (call "get_writing_context" {"id" id})
        r (get res "result")
        ctx (get r "structuredContent")]
    (is (= id (get ctx "card_id")))
    (is (pos? (get ctx "chars")))
    (is (= (get ctx "chars")
           (reduce + (for [s (get ctx "sections") e (get s "entries")]
                       (+ (count (get e "label")) (count (get e "text")))))))
    (is (every? #{"upper" "self" "neighbors" "children" "relations" "sources" "style"}
                (map #(get % "group") (get ctx "sections"))))
    (is (some #(= "self" (get % "group")) (get ctx "sections")))
    (is (some #(= "intent" (get-in % ["entries" 0 "source"])) (get ctx "sections")))
    (is (= ctx (json/read-value (get-in r ["content" 0 "text"])))
        "text 與 structuredContent 為同一份結果"))
  (testing "標題不會撰寫本文"
    (let [r (get (call "get_writing_context" {"id" (card-id "第一章 計畫概述")}) "result")]
      (is (true? (get r "isError")))
      (is (= "只有段落與引言會撰寫本文" (get-in r ["content" 0 "text"]))))))

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
