(ns knot.mcp-sources-test
  "MCP 的素材與事實工具（SPEC 5.2、5.5、5.12）。"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [jsonista.core :as json]
            [knot.context :as context]
            [knot.db :as db]
            [knot.seed :as seed]
            [knot.server :as server]
            [knot.ui :as ui]))

(def ^:dynamic *handler* nil)
(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [t]
    (let [dir (str (java.nio.file.Files/createTempDirectory "knot-mcp-src" (make-array java.nio.file.attribute.FileAttribute 0)))
          conn (db/open-report dir)]
      (try
        (seed/seed! conn (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))
        (binding [*handler* (server/handler conn) *conn* conn] (t))
        (finally (db/close-report conn))))))

(defn- call [tool args]
  (let [res (*handler* {:request-method :post :uri "/mcp"
                        :headers {"content-type" "application/json"}
                        :body (java.io.ByteArrayInputStream.
                               (.getBytes ^String (json/write-value-as-string
                                                   {"jsonrpc" "2.0" "id" 1 "method" "tools/call"
                                                    "params" {"name" tool "arguments" args}})
                                          "UTF-8"))})]
    (json/read-value (str (:body res)))))

(defn- ok
  "成功的結果（structuredContent）；失敗時測試不通過並回傳 nil。"
  [res]
  (is (not (true? (get-in res ["result" "isError"]))) (str (get-in res ["result" "content" 0 "text"])))
  (get-in res ["result" "structuredContent"]))

(defn- err
  "錯誤說明文字；成功時測試不通過。"
  [res]
  (is (true? (get-in res ["result" "isError"])))
  (get-in res ["result" "content" 0 "text"]))

(defn- state [] (ui/from-server (db/load-state (d/db *conn*))))

(defn- card-id [title]
  (str (:node/id (some #(when (or (= title (:node/title %)) (= title (:node/name %))) %) (vals (:nodes (state)))))))

(defn- changesets []
  (let [db (d/db *conn*)]
    (->> (d/q '[:find ?id ?at :where [?c :changeset/id ?id] [?c :changeset/at ?at]] db)
         (sort-by second)
         (mapv #(d/pull db [:changeset/id :changeset/by :changeset/label :changeset/group] [:changeset/id (first %)])))))

(defn- block-id [title]
  (str (:block/id (some #(when (= title (:block/title %)) %) (:blocks (state))))))

(defn- add-excerpt! [m]
  (get-in (ok (call "add_excerpt" (merge {"source" "期末報告.pdf" "title" "總結" "text" "路燈共 120 盞。"} m)))
          ["blocks" 0 "block_id"]))

(deftest add-excerpt-tool-test
  (let [before (count (changesets))
        res (ok (call "add_excerpt" {"source" "期末報告.pdf" "title" "計畫目標" "location" "p.3"
                                     "text" "本計畫目標為建置智慧路燈，共 120 盞。" "note" "第一章用"
                                     "summary" "節錄計畫目標"}))
        bid (get-in res ["blocks" 0 "block_id"])]
    (testing "回傳素材與修改組，修改組是 agent 的、名稱用 summary"
      (is (uuid? (parse-uuid bid)))
      (is (= {"kind" "para" "title" "計畫目標" "source" "期末報告.pdf" "location" "p.3" "status" "confirmed"}
             (dissoc (first (get res "blocks")) "block_id")))
      (is (string? (get res "changeset_id")))
      (is (= (inc before) (count (changesets))))
      (is (= [:agent "節錄計畫目標"] ((juxt :changeset/by :changeset/label) (last (changesets))))))
    (testing "立即可引用：狀態是 confirmed，note 存下來"
      (let [b (some #(when (= bid (str (:block/id %))) %) (:blocks (state)))]
        (is (= :confirmed (:block/status b)))
        (is (= "第一章用" (:block/note b)))))
    (testing "復原後素材消失"
      (db/undo! *conn*)
      (is (nil? (some #(when (= bid (str (:block/id %))) %) (:blocks (state))))))
    (testing "錯誤：缺欄位、重複"
      (is (re-find #"text" (err (call "add_excerpt" {"source" "x" "title" "t"}))))
      (is (re-find #"source" (err (call "add_excerpt" {"title" "t" "text" "x"}))))
      (add-excerpt! {"text" "唯一的一段。"})
      (is (re-find #"已有相同原文" (err (call "add_excerpt" {"source" "期末報告.pdf" "title" "別名" "text" "唯一的一段。"})))))))

(deftest add-source-table-tool-test
  (let [res (ok (call "add_source_table" {"source" "量測.xlsx" "title" "年用電" "columns" ["場域" "年用電 (kWh)"]
                                          "rows" [["A" 192920] ["B" "156,000"]] "group" "整理素材"}))
        bid (get-in res ["blocks" 0 "block_id"])
        b (some #(when (= bid (str (:block/id %))) %) (:blocks (state)))]
    (is (= "table" (get-in res ["blocks" 0 "kind"])))
    (is (= [["A" "192920"] ["B" "156,000"]] (:table/rows b)))
    (is (= "整理素材" (:changeset/group (last (changesets)))) "group 是修改組在歷程中的名稱")
    (testing "錯誤"
      (is (re-find #"2 欄" (err (call "add_source_table" {"source" "x" "title" "t" "columns" ["a" "b"] "rows" [["1"]]}))))
      (is (re-find #"columns" (err (call "add_source_table" {"source" "x" "title" "t" "columns" [] "rows" []})))))))

(deftest add-fact-tool-test
  (let [bid (add-excerpt! {"text" "本計畫共建置 2,871 盞路燈，平均節電率 78.4%。"})
        tid (block-id "各場域量測結果")]
    (testing "節錄上的事實；值與單位分開"
      (let [r (ok (call "add_fact" {"key" "lights.total" "label" "路燈數" "value" "2,871" "unit" "盞" "block_id" bid}))]
        (is (= [{"key" "lights.total" "label" "路燈數" "value" "2,871" "display" "2,871 盞" "unit" "盞" "block_id" bid "block_title" "總結"}]
               (get r "facts"))))
      (ok (call "add_fact" {"key" "saving.avg" "label" "平均節電率" "value" 78.4 "unit" "%" "block_id" bid})))
    (testing "表格儲存格：回傳列與欄；寫入資料庫"
      (let [r (ok (call "add_fact" {"key" "rate.c" "label" "C 節電率" "value" "18" "unit" "%" "block_id" tid
                                    "row" 2 "column" "節電率"}))]
        (is (= [2 "節電率"] ((juxt #(get % "row") #(get % "column")) (first (get r "facts")))))
        (is (= [2 "節電率"] ((juxt :fact/row :fact/column) (get (:facts (state)) "rate.c"))))))
    (testing "值不在素材中：拒絕並說明；沒有寫入"
      (let [n (count (changesets))]
        (is (re-find #"2872" (err (call "add_fact" {"key" "bad.value" "label" "x" "value" "2872" "block_id" bid}))))
        (is (re-find #"18%" (err (call "add_fact" {"key" "bad.cell" "label" "x" "value" "47" "block_id" tid "row" 2 "column" "節電率"}))))
        (is (= n (count (changesets))))
        (is (not (contains? (:facts (state)) "bad.value")))))
    (testing "其他錯誤"
      (is (re-find #"格式不正確" (err (call "add_fact" {"key" "Bad_Key" "label" "x" "value" "78.4" "block_id" bid}))))
      (is (re-find #"已經存在" (err (call "add_fact" {"key" "lights.total" "label" "x" "value" "78.4" "block_id" bid}))))
      (is (re-find #"block_id" (err (call "add_fact" {"key" "k" "label" "x" "value" "78.4"}))))
      (is (re-find #"找不到素材" (err (call "add_fact" {"key" "k" "label" "x" "value" "1" "block_id" (str (random-uuid))}))))
      (is (re-find #"UUID" (err (call "add_fact" {"key" "k" "label" "x" "value" "1" "block_id" "abc"})))))
    (testing "list_facts：全部與依素材篩選"
      (is (= #{"saving-rate.site-a" "saving-rate.site-b" "saving-rate.site-c" "lights.total" "saving.avg" "rate.c"}
             (set (map #(get % "key") (get (ok (call "list_facts" {})) "facts")))))
      (is (= ["lights.total" "saving.avg"]
             (mapv #(get % "key") (get (ok (call "list_facts" {"block_id" bid})) "facts"))))
      (is (re-find #"找不到素材" (err (call "list_facts" {"block_id" (str (random-uuid))})))))))

(deftest search-sources-tool-test
  (let [res (ok (call "search_sources" {"query" "節電"}))
        titles (mapv #(get % "title") (get res "results"))]
    (testing "搜尋「節電」找到含「節電率」的素材（名稱、欄名）"
      (is (some #{"各場域量測結果"} titles)))
    (testing "每筆的欄位：表格有 columns 與 row_count、節錄有 excerpt；事實數"
      (let [t (some #(when (= "各場域量測結果" (get % "title")) %) (get res "results"))
            p (some #(when (= "故障通報時效" (get % "title")) %) (get (ok (call "search_sources" {"query" ""})) "results"))]
        (is (= ["場域" "節電率" "斷線次數"] (get t "columns")))
        (is (= 3 (get t "row_count")))
        (is (= 3 (get t "fact_count")))
        (is (= "table" (get t "kind")))
        (is (= "導入自動通報後，故障平均發現時間由 3.5 天縮短為 4 小時。" (get p "excerpt")))
        (is (= 0 (get p "fact_count")))
        (is (= "p.3" (get p "location")))))
    (testing "sources 列出所有來源檔名與素材數"
      (is (= {"量測紀錄.xlsx" 2 "期中報告.pdf" 2 "期末量測紀錄.md" 1}
             (select-keys (into {} (map (juxt #(get % "source") #(get % "count"))) (get res "sources"))
                          ["量測紀錄.xlsx" "期中報告.pdf" "期末量測紀錄.md"]))))
    (testing "空字串列出全部；kind、source 篩選；limit"
      (is (= 8 (get (ok (call "search_sources" {"query" ""})) "total")))
      (is (= #{"table"} (set (map #(get % "kind") (get (ok (call "search_sources" {"query" "" "kind" "table"})) "results")))))
      (is (= #{"量測紀錄.xlsx"} (set (map #(get % "source") (get (ok (call "search_sources" {"query" "" "source" "量測紀錄.xlsx"})) "results")))))
      (let [r (ok (call "search_sources" {"query" "" "limit" 2}))]
        (is (= 2 (count (get r "results"))))
        (is (= 8 (get r "total")))
        (is (re-find #"前 2 筆" (get r "message")))))
    (testing "多個詞全部都要出現；沒有結果時有提示且仍列出 sources"
      (is (= ["故障通報時效"] (mapv #(get % "title") (get (ok (call "search_sources" {"query" "通報 4小時"})) "results"))))
      (let [r (ok (call "search_sources" {"query" "不存在的詞"}))]
        (is (= [] (get r "results")))
        (is (string? (get r "message")))
        (is (seq (get r "sources")))))
    (testing "excerpt 最多 120 字"
      (let [bid (add-excerpt! {"title" "很長" "text" (apply str (repeat 300 "字"))})
            r (some #(when (= bid (get % "block_id")) %) (get (ok (call "search_sources" {"query" "很長"})) "results"))]
        (is (= 121 (count (get r "excerpt"))))))
    (testing "錯誤"
      (is (re-find #"para、table" (err (call "search_sources" {"query" "" "kind" "figure"}))))
      (is (re-find #"limit" (err (call "search_sources" {"query" "" "limit" 0}))))
      (is (re-find #"query" (err (call "search_sources" {"query" 1})))))))

(deftest get-source-block-tool-test
  (let [tid (block-id "各場域量測結果")
        p312 (card-id "各場域結果比較")]
    (ok (call "add_citation" {"card_id" p312 "block_id" tid}))
    (let [r (ok (call "get_source_block" {"block_id" tid}))]
      (is (= ["場域" "節電率" "斷線次數"] (get r "columns")))
      (is (= [["場域 A" "47%" "2"] ["場域 B" "40%" "3"] ["場域 C" "18%" "17"]] (get r "rows")))
      (is (= ["saving-rate.site-a" "saving-rate.site-b" "saving-rate.site-c"] (mapv #(get % "key") (get r "facts"))))
      (is (= [p312] (mapv #(get % "card_id") (get r "cited_by"))))
      (is (= "各場域結果比較" (get-in r ["cited_by" 0 "title"])))
      (is (string? (get-in r ["cited_by" 0 "position"]))))
    (let [r (ok (call "get_source_block" {"block_id" (block-id "故障通報時效")}))]
      (is (= "導入自動通報後，故障平均發現時間由 3.5 天縮短為 4 小時。" (get r "text")))
      (is (= [] (get r "facts"))))
    (is (re-find #"找不到素材" (err (call "get_source_block" {"block_id" (str (random-uuid))}))))
    (is (re-find #"缺少參數 block_id" (err (call "get_source_block" {}))))))

(deftest update-source-block-and-fact-tool-test
  (let [bid (add-excerpt! {"text" "節電率達 47%。"})
        p (card-id "各場域結果比較")]
    (ok (call "add_fact" {"key" "rate.x" "label" "節電率" "value" "47" "unit" "%" "block_id" bid}))
    (ok (call "write_prose" {"card_id" p "text" (str "節電率達 {{fact:rate.x}}。[[cite:" bid "]]")}))
    (testing "改原文使事實對不上：寫入並回報 fact_problems"
      (let [r (ok (call "update_source_block" {"block_id" bid "text" "節電率達 48%。"}))]
        (is (= ["rate.x"] (mapv #(get % "key") (get r "fact_problems"))))
        (is (re-find #"update_fact" (get r "message")))
        (is (= "節電率達 48%。" (:block/text (some #(when (= bid (str (:block/id %))) %) (:blocks (state))))))))
    (testing "update_fact 改成新值：本文以新值顯示，不必改本文"
      (let [r (ok (call "update_fact" {"key" "rate.x" "value" "48"}))
            prose-before (:node/prose (get (:nodes (state)) (parse-uuid p)))]
        (is (= p (get-in r ["cited_cards" 0 "card_id"])))
        (is (= "48" (get-in (state) [:facts "rate.x" :fact/value])))
        (let [st (state)]
          (is (str/includes? (context/prose-text (:facts st) (:node/prose (get (:nodes st) (parse-uuid p)))) "節電率達 48%。"))
          (is (= prose-before (:node/prose (get (:nodes st) (parse-uuid p)))) "本文沒有改"))))
    (testing "update_fact 的錯誤"
      (is (re-find #"沒有出現" (err (call "update_fact" {"key" "rate.x" "value" "99"}))))
      (is (re-find #"找不到事實" (err (call "update_fact" {"key" "nope" "value" "1"}))))
      (is (re-find #"沒有要修改" (err (call "update_fact" {"key" "rate.x"})))))
    (testing "update_source_block 的錯誤"
      (is (re-find #"沒有要修改" (err (call "update_source_block" {"block_id" bid}))))
      (is (re-find #"找不到素材" (err (call "update_source_block" {"block_id" (str (random-uuid)) "title" "x"})))))
    (testing "改成不指定儲存格：row、column 傳 null"
      (let [tid (block-id "各場域量測結果")]
        (ok (call "update_fact" {"key" "saving-rate.site-b" "row" 1 "column" "節電率"}))
        (is (= [1 "節電率"] ((juxt :fact/row :fact/column) (get (:facts (state)) "saving-rate.site-b"))))
        (ok (call "update_fact" {"key" "saving-rate.site-b" "row" nil "column" nil}))
        (is (nil? (:fact/row (get (:facts (state)) "saving-rate.site-b"))))
        (is (= tid (str (:fact/block (get (:facts (state)) "saving-rate.site-b")))))))))

(deftest delete-fact-tool-test
  (let [bid (add-excerpt! {"text" "節電率達 47%。"})
        p (card-id "各場域結果比較")]
    (ok (call "add_fact" {"key" "rate.x" "label" "節電率" "value" "47" "unit" "%" "block_id" bid}))
    (ok (call "write_prose" {"card_id" p "text" "節電率達 {{fact:rate.x}}。"}))
    (testing "本文還用到時拒絕並列出段落"
      (is (re-find #"各場域結果比較" (err (call "delete_fact" {"key" "rate.x"})))))
    (testing "改寫本文後可以刪除，並可復原"
      (ok (call "write_prose" {"card_id" p "text" "節電率達 47%。"}))
      (ok (call "delete_fact" {"key" "rate.x"}))
      (is (nil? (get (:facts (state)) "rate.x")))
      (db/undo! *conn*)
      (is (= "47" (get-in (state) [:facts "rate.x" :fact/value]))))
    (testing "錯誤"
      (is (re-find #"找不到事實" (err (call "delete_fact" {"key" "nope"}))))
      (is (re-find #"key" (err (call "delete_fact" {})))))))

(deftest citation-tools-test
  (let [bid (add-excerpt! {})
        p (card-id "各場域結果比較")
        cites #(mapv (fn [c] [(str (:block c)) (:where c)]) (:node/citations (get (:nodes (state)) (parse-uuid p))))]
    (testing "加入依據，回傳卡片"
      (let [r (ok (call "add_citation" {"card_id" p "block_id" bid "where" "第一段"}))]
        (is (= [p] (mapv #(get % "card_id") (get r "cards"))))
        (is (some #{[bid "第一段"]} (cites)))))
    (testing "get_card 的依據"
      (is (some #(= bid (get % "block_id")) (get (ok (call "get_card" {"card_id" p})) "citations"))))
    (testing "重複：已經是依據，沒有寫入"
      (let [n (count (changesets))
            r (ok (call "add_citation" {"card_id" p "block_id" bid}))]
        (is (re-find #"已經是" (get r "message")))
        (is (nil? (get r "changeset_id")))
        (is (= n (count (changesets))))))
    (testing "只限段落與引言"
      (is (re-find #"段落與引言" (err (call "add_citation" {"card_id" (card-id "計畫概述") "block_id" bid}))))
      (is (re-find #"找不到卡片" (err (call "add_citation" {"card_id" (str (random-uuid)) "block_id" bid})))))
    (testing "移除依據"
      (ok (call "remove_citation" {"card_id" p "block_id" bid}))
      (is (not-any? #(= bid (first %)) (cites)))
      (is (re-find #"不是" (err (call "remove_citation" {"card_id" p "block_id" bid})))))))

(deftest discard-flow-test
  (let [bid (add-excerpt! {"title" "總燈數" "text" "全市共 120 盞路燈。"})
        p (card-id "各場域結果比較")
        search #(mapv (fn [r] (get r "block_id")) (get (ok (call "search_sources" {"query" "總燈數"})) "results"))]
    (ok (call "add_fact" {"key" "lamps" "label" "路燈數" "value" "120" "block_id" bid}))
    (ok (call "write_prose" {"card_id" p "text" (str "全市有 {{fact:lamps}} 盞。[[cite:" bid "]]")}))
    (testing "[[cite:...]] 加入依據"
      (is (some #(= bid (get % "block_id")) (get (ok (call "get_card" {"card_id" p})) "citations"))))
    (is (= [bid] (search)))
    (testing "捨棄：回傳仍引用它的段落；搜尋不到；get_card 標示 discarded"
      (let [r (ok (call "discard_source_block" {"block_id" bid}))]
        (is (= [p] (mapv #(get % "card_id") (get r "cited_cards"))))
        (is (= "discarded" (get-in r ["blocks" 0 "status"]))))
      (is (= [] (search)))
      (is (true? (some #(when (= bid (get % "block_id")) (get % "discarded")) (get (ok (call "get_card" {"card_id" p})) "citations"))))
      (is (true? (some #(when (:discarded %) true) (mapcat :entries (:sections (context/generation-context (state) (parse-uuid p))))))
          "撰寫參考的依據標示已捨棄")
      (is (true? (some #(get % "discarded") (mapcat #(get % "entries") (get (ok (call "get_writing_context" {"card_id" p})) "sections"))))
          "get_writing_context 也標示")
      (is (= "discarded" (get (ok (call "get_source_block" {"block_id" bid})) "status")) "仍可用 get_source_block 查看"))
    (testing "捨棄的素材不能引用、新增事實、修改、再捨棄"
      (is (re-find #"已捨棄" (err (call "write_prose" {"card_id" p "text" (str "新 [[cite:" bid "]]")}))))
      (is (re-find #"已被捨棄" (err (call "add_citation" {"card_id" p "block_id" bid}))))
      (is (re-find #"已被捨棄" (err (call "add_fact" {"key" "lamps2" "label" "x" "value" "120" "block_id" bid}))))
      (is (re-find #"已被捨棄" (err (call "update_source_block" {"block_id" bid "title" "x"}))))
      (is (re-find #"已經是捨棄" (err (call "discard_source_block" {"block_id" bid})))))
    (testing "write_prose 的錯誤列出可引用的素材（不含已捨棄）"
      (let [msg (err (call "write_prose" {"card_id" p "text" (str "[[cite:" bid "]]")}))]
        (is (not (re-find (re-pattern (str bid "（")) msg)))
        (is (re-find #"各場域量測結果" msg))))
    (testing "依據仍可移除"
      (ok (call "remove_citation" {"card_id" p "block_id" bid})))
    (testing "復原：捨棄與移除依據各復原一次後搜得到、可引用"
      (db/undo! *conn*)
      (db/undo! *conn*)
      (is (= [bid] (search)))
      (ok (call "add_citation" {"card_id" p "block_id" bid "where" "again"})))))

(deftest group-test
  (let [_ (ok (call "add_excerpt" {"source" "a" "title" "一" "text" "第一段" "group" "整理素材"}))
        r (ok (call "add_excerpt" {"source" "a" "title" "二" "text" "第二段" "group" "整理素材"}))
        n (count (changesets))]
    (is (string? (get r "changeset_id")))
    (db/undo! *conn*)
    (is (= (inc n) (count (changesets))) "整組只新增一個復原修改組")
    (is (empty? (filter #(#{"一" "二"} (:block/title %)) (:blocks (state)))) "兩筆一起復原")))

(deftest batch-tools-test
  (let [bid (add-excerpt! {"text" "共 1,210 人次，節電率 78.4%。"})
        n0 (count (changesets))
        fact-keys #(set (keys (:facts (state))))
        base (fact-keys)]
    (testing "add_facts：整批一個修改組，一次復原；回傳 display"
      (let [r (ok (call "add_facts" {"facts" [{"key" "n.1" "label" "人次" "value" "1210" "unit" "人次" "block_id" bid}
                                              {"key" "r.1" "label" "節電率" "value" "78.4" "unit" "%" "block_id" bid}]
                                     "group" "批次"}))]
        (is (= ["1,210 人次" "78.4%"] (mapv #(get % "display") (get r "facts"))))
        (is (= ["1210" "78.4"] (mapv #(get % "value") (get r "facts"))) "儲存的值不變")
        (is (= (inc n0) (count (changesets))))
        (db/undo! *conn*)
        (is (= base (fact-keys)))))
    (testing "add_facts：有錯誤整批不寫"
      (let [t (err (call "add_facts" {"facts" [{"key" "x.1" "label" "甲" "value" "1210" "block_id" bid}
                                               {"key" "x.2" "label" "乙" "value" "999" "block_id" bid}
                                               {"key" "x.1" "label" "丙" "value" "78.4" "block_id" "not-a-uuid"}]}))]
        (is (re-find #"facts\[1\]（x\.2）" t))
        (is (re-find #"facts\[2\]（x\.1）" t))
        (is (= base (fact-keys)))))
    (testing "add_excerpts：一個修改組"
      (let [before (count (:blocks (state)))
            n (count (changesets))
            r (ok (call "add_excerpts" {"excerpts" [{"source" "b.pdf" "title" "一" "text" "第一"}
                                                    {"source" "b.pdf" "title" "二" "text" "第二"}]}))]
        (is (= 2 (count (get r "blocks"))))
        (is (= (inc n) (count (changesets))))
        (db/undo! *conn*)
        (is (= before (count (:blocks (state)))))))
    (testing "add_excerpts：同批重複"
      (is (re-find #"excerpts\[1\]" (err (call "add_excerpts" {"excerpts" [{"source" "b.pdf" "title" "一" "text" "同"}
                                                                           {"source" "b.pdf" "title" "二" "text" "同"}]})))))))
