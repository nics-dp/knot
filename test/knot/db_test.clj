(ns knot.db-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [knot.db :as db]
            [knot.seed :as seed]
            [knot.sync :as sync]
            [knot.tree :as tree]
            [knot.ui :as ui]))

(def ^:dynamic *conn* nil)
(def ^:dynamic *dir* nil)

(defn- temp-dir []
  (str (java.nio.file.Files/createTempDirectory "knot-test" (make-array java.nio.file.attribute.FileAttribute 0))))

(use-fixtures :each
  (fn [t]
    (let [dir (temp-dir)
          conn (db/open-report dir)]
      (try
        (binding [*conn* conn *dir* dir] (t))
        (finally (db/close-report conn))))))

(def fixture (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))

(defn- seeded []
  (seed/seed! *conn* fixture)
  [nil *conn* (db/load-state (d/db *conn*))])

(defn- by [k coll] (into {} (map (juxt k identity)) coll))

(defn- client-state
  "把 load-state 的結果轉成前端 knot.ui 的狀態形式。"
  [loaded]
  {:nodes (by :node/id (:nodes loaded))
   :relations (:relations loaded)
   :blocks (:blocks loaded)
   :facts (by :fact/key (:facts loaded))
   :ui {:selected (:node/id (first (:nodes loaded)))}
   :history []})

(deftest seed-roundtrip-test
  (let [{:keys [nodes relations blocks facts] :as st} (seed/fixture->state fixture)
        _ (seed/import-state! *conn* st)
        loaded (db/load-state (d/db *conn*))]
    (testing "寫入後讀回的內容與寫入時相同（版本號由資料庫管理）"
      (is (= (by :node/id nodes) (update-vals (by :node/id (:nodes loaded)) #(dissoc % :node/version)))))
    (is (= (set relations) (set (:relations loaded))))
    (is (= (by :block/id blocks) (by :block/id (:blocks loaded))))
    (is (= (by :fact/key facts) (by :fact/key (:facts loaded))))
    (is (= "OO 市智慧路燈示範計畫" (get-in loaded [:report :report/title])))))

(deftest changes-and-undo-test
  (let [[_ c loaded] (seeded)
        before (client-state loaded)
        p312 (some #(when (= "各場域結果比較" (:node/title %)) (:node/id %)) (:nodes loaded))
        s311 (some #(when (= "問題與改善" (:node/name %)) (:node/id %)) (:nodes loaded))
        moved (ui/apply-move before p312 {:parent s311 :index 0})
        changes (sync/diff before moved)]
    (is (some? (db/apply-changes! c {:origin :move :label "移動" :changes changes})))
    (let [after (db/load-state (d/db c))]
      (testing "移動後讀回：parent 改變、關係標為待確認"
        (is (= s311 (:node/parent (some #(when (= p312 (:node/id %)) %) (:nodes after)))))
        (is (= (set (:relations moved)) (set (:relations after))))))
    (testing "復原後回到移動前"
      (is (= "移動" (db/undo! c)))
      (let [undone (db/load-state (d/db c))]
        (is (= (update-vals (:nodes before) #(dissoc % :node/version))
               (update-vals (by :node/id (:nodes undone)) #(dissoc % :node/version))))
        (is (= (set (:relations before)) (set (:relations undone))))))
    (testing "匯入不能復原；沒有可復原的修改組時回傳 nil"
      (is (nil? (db/undo! c))))
    (testing "修訂只增不改：修改組保留在歷史中"
      (is (= 3 (count (d/q '[:find [?c ...] :where [?c :changeset/id]] (d/db c))))))))

(deftest open-report-test
  (testing "空目錄建立空白報告，名稱取自目錄名稱"
    (let [info (db/report-info (d/db *conn*))]
      (is (uuid? (:report/id info)))
      (is (= (.getName (java.io.File. *dir*)) (:report/title info)))
      (is (db/empty-report? (d/db *conn*)))))
  (testing "已有內容的報告不能再匯入範例"
    (seed/seed! *conn* fixture)
    (is (thrown? clojure.lang.ExceptionInfo (seed/seed! *conn* fixture))))
  (testing "重新開啟讀到同一份報告"
    (let [id (:report/id (db/report-info (d/db *conn*)))]
      (db/close-report *conn*)
      (let [c (db/open-report *dir*)]
        (try
          (is (= id (:report/id (db/report-info (d/db c)))))
          (is (= 47 (count (:nodes (db/load-state (d/db c))))))
          (finally (db/close-report c)))))))

(deftest delete-and-version-test
  (let [[_ c loaded] (seeded)
        state (client-state loaded)
        heading (some #(when (= "實證成果" (:node/name %)) (:node/id %)) (:nodes loaded))
        para (some #(when (= "節能效益估算" (:node/title %)) (:node/id %)) (:nodes loaded))]
    (testing "內容變更時版本加一"
      (db/apply-changes! c {:label "編輯意圖"
                            :changes (sync/diff state (ui/set-node-field state para :node/draft "新的意圖"))})
      (let [n (some #(when (= para (:node/id %)) %) (:nodes (db/load-state (d/db c))))]
        (is (= "新的意圖" (:node/draft n)))
        (is (= 2 (:node/version n)))))
    (testing "刪除標題連同下層卡片，復原後全部回來"
      (let [state (client-state (db/load-state (d/db c)))
            deleted (ui/delete-node state heading)]
        (db/apply-changes! c {:label "刪除" :changes (sync/diff state deleted)})
        (is (= (count (:nodes deleted)) (count (:nodes (db/load-state (d/db c))))))
        (db/undo! c)
        (is (= (count (:nodes state)) (count (:nodes (db/load-state (d/db c))))))
        (is (some #(= para (:node/id %)) (:nodes (db/load-state (d/db c)))))))))

(deftest rules-test
  (let [[_ c loaded] (seeded)
        id (fn [k v] (some #(when (= v (get % k)) (:node/id %)) (:nodes loaded)))
        p312 (id :node/title "各場域結果比較")
        p313 (id :node/title "異常狀況說明")
        p321 (id :node/title "節能效益估算")
        p411 (id :node/title "主要成果")
        intro3 (some #(when (and (= :intro (:node/kind %)) (= (id :node/name "執行成果") (:node/parent %)))
                        (:node/id %))
                     (:nodes loaded))
        db (d/db c)]
    (is (= #{(id :node/name "執行成果") (id :node/name "測試結果")} (db/ancestor-ids db p312)))
    (testing "摘要依賴：摘要關係與上層標題的引言"
      (is (= #{p321 p411 intro3} (:summary (db/dependents db p312)))))
    (testing "論證依賴：下一段（因果）"
      (is (= #{p313} (:argument (db/dependents db p312)))))
    (testing "依據關係也是論證依賴"
      (let [state (client-state loaded)]
        (db/apply-changes! c {:label "新增關係"
                              :changes (sync/diff state (ui/add-remote-relation state p411 p313 :basis))})
        (is (contains? (:argument (db/dependents (d/db c) p313)) p411))))
    (testing "樹的順序與編號由 order-key 計算"
      (is (= "3.1" (get (tree/numbering (by :node/id (:nodes loaded))) (id :node/name "測試結果")))))))
