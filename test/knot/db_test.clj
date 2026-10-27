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
        heading (some #(when (= "問題與改善" (:node/name %)) (:node/id %)) (:nodes loaded))
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

(defn- prose [text]
  {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text}]}]})

(deftest node-previous-test
  (let [[_ c loaded] (seeded)
        para (some #(when (= "節能效益估算" (:node/title %)) (:node/id %)) (:nodes loaded))
        original (some #(when (= para (:node/id %)) (:node/prose %)) (:nodes loaded))
        save! (fn [label f]
                (let [state (client-state (db/load-state (d/db c)))]
                  (db/apply-changes! c {:label label :changes (sync/diff state (f state))})))]
    (testing "匯入之後還沒修改過"
      (is (= {} (db/node-previous (d/db c) para))))
    (testing "意圖不需要歷程"
      (save! "編輯文字" #(ui/set-node-field % para :node/draft "新的意圖"))
      (is (= {} (db/node-previous (d/db c) para))))
    (testing "連續的逐字輸入算同一次修改"
      (save! "編輯文字" #(ui/set-prose % para (prose "新")))
      (save! "編輯文字" #(ui/set-prose % para (prose "新的本文")))
      (is (= original (get-in (db/node-previous (d/db c) para) [:node/prose :value]))))
    (testing "其他修改組不合併"
      (save! "重新生成" #(ui/set-prose % para (prose "生成的本文")))
      (is (= (prose "新的本文") (get-in (db/node-previous (d/db c) para) [:node/prose :value]))))))

(defn- node-of [loaded id] (some #(when (= id (:node/id %)) %) (:nodes loaded)))

(deftest changeset-by-test
  (let [[_ c loaded] (seeded)
        state (client-state loaded)
        para (some #(when (= "節能效益估算" (:node/title %)) (:node/id %)) (:nodes loaded))
        edit (fn [text opts]
               (let [state (client-state (db/load-state (d/db c)))]
                 (db/apply-changes! c (merge {:label text
                                              :changes (sync/diff state (ui/set-node-field state para :node/draft text))}
                                             opts))))
        by-of (fn [id] (:changeset/by (d/pull (d/db c) [:changeset/by] [:changeset/id id])))]
    (is (= :user (by-of (:changeset/id (edit "a" {})))) "沒給 :by 時寫 :user")
    (is (= :agent (by-of (:changeset/id (edit "b" {:by :agent})))))
    (is (some? state))))

(deftest version-check-test
  (let [[_ c loaded] (seeded)
        para (some #(when (= "節能效益估算" (:node/title %)) (:node/id %)) (:nodes loaded))
        edit (fn [state text opts]
               (db/apply-changes! c (merge {:label text
                                            :changes (sync/diff state (ui/set-node-field state para :node/draft text))}
                                           opts)))
        state (client-state loaded)]
    (testing "未衝突：寫入並回傳新版本"
      (let [res (edit state "第一次" {:check-versions? true})]
        (is (uuid? (:changeset/id res)))
        (is (= {para 2} (:versions res)))
        (is (= 2 (:node/version (node-of (db/load-state (d/db c)) para))))))
    (testing "連續存檔：第二次帶第一次回傳的版本，不衝突"
      (let [state2 (-> state
                       (ui/set-node-field para :node/draft "第一次")
                       (assoc-in [:nodes para :node/version] 2))
            res (edit state2 "第二次" {:check-versions? true})]
        (is (nil? (:conflicts res)))
        (is (= {para 3} (:versions res)))))
    (testing "版本過期：拒絕，資料庫完全沒變"
      (let [before (db/load-state (d/db c))
            n-changesets (count (d/q '[:find [?c ...] :where [?c :changeset/id]] (d/db c)))
            res (edit state "過期的修改" {:check-versions? true})]
        (is (= {:conflicts [{:node/id para :expected 1 :actual 3}]} res))
        (is (= before (db/load-state (d/db c))))
        (is (= n-changesets (count (d/q '[:find [?c ...] :where [?c :changeset/id]] (d/db c)))))))
    (testing "沒開檢查時照寫"
      (is (uuid? (:changeset/id (edit state "不檢查" {})))))
    (testing "復原不受檢查影響"
      (is (= "復原：不檢查" (str "復原：" (db/undo! c)))))))

(deftest new-and-deleted-nodes-skip-check-test
  (let [[_ c loaded] (seeded)
        state (client-state loaded)
        heading (some #(when (= "問題與改善" (:node/name %)) (:node/id %)) (:nodes loaded))
        deleted (ui/delete-node state heading)]
    (testing "刪除不檢查版本"
      (let [res (db/apply-changes! c {:label "刪除" :check-versions? true
                                      :changes (sync/diff state deleted)})]
        (is (uuid? (:changeset/id res)))))
    (testing "新卡片（資料庫沒有）不檢查"
      (let [id (random-uuid)
            node {:node/id id :node/kind :paragraph :node/parent nil :node/order-key "z" :node/version 99}
            res (db/apply-changes! c {:label "新增" :check-versions? true
                                      :changes {:nodes {:upsert [node]}}})]
        (is (nil? (:conflicts res)))
        (is (contains? (:versions res) id))))))

(deftest version-bumps-on-any-field-test
  (let [[_ c loaded] (seeded)
        state (client-state loaded)
        para (some #(when (= "節能效益估算" (:node/title %)) (:node/id %)) (:nodes loaded))
        moved (assoc-in state [:nodes para :node/order-key] "zz")
        res (db/apply-changes! c {:label "移動" :check-versions? true :changes (sync/diff state moved)})]
    (testing "只改位置也加一，之後帶舊版本的寫入被擋下"
      (is (= {para 2} (:versions res)))
      (is (= [{:node/id para :expected 1 :actual 2}]
             (:conflicts (db/apply-changes! c {:check-versions? true
                                               :changes (sync/diff state (ui/set-node-field state para :node/draft "x"))})))))))
