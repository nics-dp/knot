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

(deftest locked-status-migrates-test
  (testing "鎖定功能已移除：寫入舊快照中的 :locked 時改為 :human"
    (let [[_ c loaded] (seeded)
          state (client-state loaded)
          para (some #(when (= "節能效益估算" (:node/title %)) (:node/id %)) (:nodes loaded))
          old (assoc-in state [:nodes para :node/status] :locked)]
      (db/apply-changes! c {:changes (sync/diff state old)})
      (is (= :human (:node/status (node-of (db/load-state (d/db c)) para)))))))

(deftest recent-cards-test
  (let [[_ c loaded] (seeded)
        id-of (fn [t] (some #(when (= t (:node/title %)) (:node/id %)) (:nodes loaded)))
        [p1 p2 p3] (map id-of ["節能效益估算" "維運成本比較" "測試方法概述"])
        edit! (fn [id text by]
                (let [state (client-state (db/load-state (d/db c)))]
                  (db/apply-changes! c {:label text :by by
                                        :changes (sync/diff state (ui/set-node-field state id :node/draft text))})))
        recent #(map (juxt :node/id :label :by) (db/recent-cards (d/db c) %))]
    (is (= [] (recent 10)) "匯入不算修改")
    (edit! p1 "agent 一" :agent)
    (edit! p2 "使用者" :user)
    (edit! p3 "agent 二" :agent)
    (edit! p1 "agent 三" :agent)
    (is (= [[p1 "agent 三" :agent] [p3 "agent 二" :agent] [p2 "使用者" :user]] (recent 10))
        "不分來源，新到舊，每張卡片只列最近一次")
    (is (= [p1] (map first (recent 1))))
    (testing "復原本身不算修改，已復原的修改組不列"
      (db/undo! c)
      (is (= [[p3 "agent 二" :agent] [p2 "使用者" :user] [p1 "agent 一" :agent]] (recent 10))))
    (testing "已刪除的卡片不列"
      (let [state (client-state (db/load-state (d/db c)))]
        (db/apply-changes! c {:label "刪除" :changes (sync/diff state (ui/delete-node state p3))}))
      (is (= [p2 p1] (map first (recent 10)))))))

(deftest node-history-test
  (let [[_ c loaded] (seeded)
        para (some #(when (= "節能效益估算" (:node/title %)) (:node/id %)) (:nodes loaded))
        other (some #(when (= "主要成果" (:node/title %)) (:node/id %)) (:nodes loaded))
        save! (fn [label f opts]
                (let [state (client-state (db/load-state (d/db c)))]
                  (db/apply-changes! c (assoc opts :label label :changes (sync/diff state (f state))))))
        hist #(db/node-history (d/db c) para)]
    (testing "匯入之後第一筆是匯入"
      (is (= [:import] (mapv :kind (hist)))))
    (testing "意圖與連續的逐字輸入"
      (save! "編輯意圖" #(ui/set-node-field % para :node/draft "新的意圖") {})
      (save! "編輯文字" #(ui/set-prose % para (prose "新")) {})
      (save! "編輯文字" #(ui/set-prose % para (prose "新的本文")) {})
      (let [[_ a b :as es] (hist)]
        (is (= 3 (count es)))
        (is (= #{:intent} (:fields a)))
        (is (= "新的意圖" (get-in a [:text-changes :intent :after])))
        (is (= 2 (:merged b)))
        (is (contains? (:fields b) :prose))))
    (testing "本文前後（合併項目的任一修改組）"
      (let [b (last (hist))
            cid (first (:changeset-ids b))
            change (db/node-change (d/db c) para cid)]
        (is (= (prose "新的本文") (:after change)))
        (is (not= (prose "新") (:before change)) "取最早的 before")
        (is (= change (db/node-change (d/db c) para (last (:changeset-ids b)))))
        (is (nil? (db/node-change (d/db c) para (:changeset/id (save! "x" identity {})))))))
    (testing "agent 修改與復原"
      (save! "重新生成" #(ui/set-prose % para (prose "agent 的本文")) {:by :agent})
      (is (= :agent (:by (last (hist)))))
      (db/undo! c)
      (let [es (hist)]
        (is (true? (:undone? (last (butlast es)))))
        (is (= :undo (:kind (last es))))
        (is (= (prose "新的本文") (:after (db/node-change (d/db c) para (first (:changeset-ids (last es)))))))))
    (testing "別的卡片的修改不混進來"
      (let [n (count (hist))]
        (save! "編輯文字" #(ui/set-node-field % other :node/draft "別張") {})
        (is (= n (count (hist))))))
    (testing "沒有修訂的卡片為空"
      (is (= [] (db/node-history (d/db c) (random-uuid)))))))

(deftest node-history-relations-test
  (let [[_ c loaded] (seeded)
        id #(some (fn [n] (when (= % (:node/title n)) (:node/id n))) (:nodes loaded))
        p411 (id "主要成果") p313 (id "節能效益估算")
        state (client-state loaded)]
    (db/apply-changes! c {:label "新增關係"
                          :changes (sync/diff state (ui/add-remote-relation state p411 p313 :basis))})
    (is (= :relations (-> (db/node-history (d/db c) p411) last :fields first)) "起點卡片")
    (is (= #{:relations} (-> (db/node-history (d/db c) p313) last :fields)) "終點卡片")))

(deftest node-history-delete-undo-test
  (let [[_ c loaded] (seeded)
        para (some #(when (= "節能效益估算" (:node/title %)) (:node/id %)) (:nodes loaded))
        state (client-state loaded)]
    (db/apply-changes! c {:label "刪除" :changes (sync/diff state (ui/delete-node state para))})
    (is (= [:import :delete] (mapv :kind (db/node-history (d/db c) para))))
    (db/undo! c)
    (let [es (db/node-history (d/db c) para)]
      (is (= [:import :delete :undo] (mapv :kind es)) "刪除後復原是新實體，但 id 相同，歷程接續")
      (is (true? (:undone? (second es)))))))

;; ---------------------------------------------------------------------------
;; 修改組分組（B1）

(defn- group-fixture
  "三張段落與一個存檔函式：(save! f opts) 把 f 套在目前狀態上寫成一個修改組。"
  [c loaded]
  (let [titles ["節能效益估算" "各場域結果比較" "主要成果"]
        ids (mapv (fn [t] (some #(when (= t (:node/title %)) (:node/id %)) (:nodes loaded))) titles)
        save! (fn [f opts]
                (let [state (client-state (db/load-state (d/db c)))]
                  (db/apply-changes! c (assoc opts :changes (sync/diff state (f state))))))]
    {:ids ids :save! save!}))

(defn- intent-of [c id]
  (some #(when (= id (:node/id %)) (:node/draft %)) (:nodes (db/load-state (d/db c)))))

(defn- changesets [c]
  (->> (d/q '[:find ?id ?at :where [?c :changeset/id ?id] [?c :changeset/at ?at]] (d/db c))
       (sort-by second)
       (mapv (fn [[id _]] (d/pull (d/db c) '[*] [:changeset/id id])))))

(deftest group-merge-and-undo-test
  (let [[_ c loaded] (seeded)
        {[a b _] :ids save! :save!} (group-fixture c loaded)
        orig-a (intent-of c a) orig-b (intent-of c b)
        agent {:by :agent :group "依審查調整" :label "x"}
        set-intent (fn [id v] #(ui/set-node-field % id :node/draft v))]
    (save! (set-intent a "A1") agent)
    (save! (set-intent b "B1") agent)
    (save! (set-intent a "A2") agent)
    (testing "同組的修改組共用 group-id"
      (let [[_ & cs] (changesets c)]
        (is (= 3 (count cs)))
        (is (= 1 (count (set (map :changeset/group-id cs)))))
        (is (every? #(= "依審查調整" (:changeset/group %)) cs))))
    (testing "每張卡片的歷程只有一筆，label 是組名"
      (let [ha (db/node-history (d/db c) a) hb (db/node-history (d/db c) b)]
        (is (= ["依審查調整"] (map :label (rest ha))))
        (is (= 2 (:merged (last ha))))
        (is (= #{:intent} (:fields (last ha))))
        (is (= {:before orig-a :after "A2"} (get-in (last ha) [:text-changes :intent])))
        (is (= ["依審查調整"] (map :label (rest hb))))))
    (testing "最近修改用組名"
      (is (= #{"依審查調整"} (set (map :label (db/recent-cards (d/db c) 5))))))
    (testing "一次復原整組，只多一個復原修改組"
      (let [n (count (changesets c))]
        (is (= "依審查調整" (db/undo! c)))
        (is (= (inc n) (count (changesets c))))
        (is (= [orig-a orig-b] [(intent-of c a) (intent-of c b)]))
        (let [undo (last (changesets c))]
          (is (= :undo (:changeset/origin undo)))
          (is (= "復原：依審查調整" (:changeset/label undo)))
          (is (some? (:changeset/undoes-group undo))))
        (is (= 3 (count (filter (db/undone-ids (d/db c)) (map :changeset/id (changesets c))))))
        (is (every? :undone? (filter #(= :change (:kind %)) (db/node-history (d/db c) a))))
        (is (empty? (db/recent-cards (d/db c) 5)) "已復原的修改組不列")
        (testing "再復原只會復原組之前的修改（這裡沒有，是匯入）"
          (is (nil? (db/undo! c))))))))

(deftest group-break-test
  (let [[_ c loaded] (seeded)
        {[a b _] :ids save! :save!} (group-fixture c loaded)
        set-intent (fn [id v] #(ui/set-node-field % id :node/draft v))
        gids #(mapv :changeset/group-id (rest (changesets c)))
        agent #(assoc {:by :agent :label "x"} :group %)]
    (testing "中間夾使用者寫入就斷開"
      (save! (set-intent a "1") (agent "G"))
      (save! (set-intent b "2") {:label "編輯意圖"})
      (save! (set-intent a "3") (agent "G"))
      (let [[g1 u g3] (gids)]
        (is (nil? u))
        (is (not= g1 g3))
        (is (= 3 (count (db/node-history (d/db c) a))) "匯入加兩筆（兩組）")
        (is (= ["G" "G"] (map :label (rest (db/node-history (d/db c) a)))))))
    (testing "不同組名斷開"
      (save! (set-intent a "4") (agent "H"))
      (save! (set-intent a "5") (agent "H"))
      (save! (set-intent a "6") (agent "I"))
      (let [gs (take-last 3 (gids))]
        (is (= (first gs) (second gs)))
        (is (not= (second gs) (last gs)))))
    (testing "超過 30 分鐘斷開"
      (let [last-id (:changeset/id (last (changesets c)))]
        (d/transact! c [{:changeset/id last-id
                         :changeset/at (java.util.Date. (- (System/currentTimeMillis) (* 31 60 1000)))}])
        (save! (set-intent a "7") (agent "I"))
        (let [gs (take-last 2 (gids))]
          (is (not= (first gs) (last gs))))))
    (testing "組被復原後不再延續"
      (save! (set-intent a "8") (agent "J"))
      (db/undo! c)
      (save! (set-intent a "9") (agent "J"))
      (let [cs (changesets c)]
        (is (not= (:changeset/group-id (nth cs (- (count cs) 3))) (:changeset/group-id (last cs))))))))

(deftest ungrouped-undo-unchanged-test
  (let [[_ c loaded] (seeded)
        {[a b _] :ids save! :save!} (group-fixture c loaded)
        oa (intent-of c a)]
    (save! #(ui/set-node-field % a :node/draft "1") {:by :agent :label "一"})
    (save! #(ui/set-node-field % b :node/draft "2") {:by :agent :label "二"})
    (is (= "二" (db/undo! c)))
    (is (= "1" (intent-of c a)))
    (is (= "一" (db/undo! c)))
    (is (= oa (intent-of c a)))
    (is (every? nil? (map :changeset/group-id (changesets c))))))

(deftest update-report-test
  (db/update-report! *conn* {:report/title "新名" :report/subtitle "副標"})
  (is (= ["新名" "副標"] ((juxt :report/title :report/subtitle) (db/report-info (d/db *conn*)))))
  (db/update-report! *conn* {:report/subtitle nil})
  (is (= ["新名" nil] ((juxt :report/title :report/subtitle) (db/report-info (d/db *conn*))))))

(deftest table-content-test
  (let [[_ c loaded] (seeded)
        t2 (some #(when (= "各場域量測結果" (:node/title %)) (:node/id %)) (:nodes loaded))
        node #(some (fn [n] (when (= t2 (:node/id n)) n)) (:nodes (db/load-state (d/db c))))
        content {:columns ["場域" "節電率"] :rows [["A" "47%"] ["B" "40%"]]}
        upsert (fn [m] (db/apply-changes! c {:origin :manual :label "表格內容"
                                             :changes {:nodes {:upsert [(merge (dissoc (node) :node/version) m)]}}}))]
    (is (nil? (:node/table-content (node))) "沒寫入時沒有這個欄位")
    (testing "寫入後讀回前端形式（map）"
      (upsert {:node/table-content content})
      (is (= content (:node/table-content (node)))))
    (testing "修改"
      (upsert {:node/table-content {:columns ["甲"] :rows []}})
      (is (= {:columns ["甲"] :rows []} (:node/table-content (node)))))
    (testing "清除：欄位從資料庫移除"
      (db/apply-changes! c {:origin :manual :label "清除"
                            :changes {:nodes {:upsert [(dissoc (node) :node/version :node/table-content)]}}})
      (is (nil? (:node/table-content (node)))))
    (testing "復原一步步回到先前的內容"
      (is (= "清除" (db/undo! c)))
      (is (= {:columns ["甲"] :rows []} (:node/table-content (node))))
      (db/undo! c)
      (is (= content (:node/table-content (node))))
      (db/undo! c)
      (is (nil? (:node/table-content (node)))))))

(deftest list-prose-extracts-facts-test
  (let [[_ c loaded] (seeded)
        p (some #(when (= "各場域結果比較" (:node/title %)) %) (:nodes loaded))
        item (fn [& content] {:type "listItem" :content [{:type "paragraph" :content (vec content)}]})
        doc {:type "doc"
             :content [{:type "paragraph" :content [{:type "text" :text "如下："}]}
                       {:type "bulletList" :content [(item {:type "fact" :attrs {:key "saving-rate.site-a"}})
                                                     (item {:type "text" :text "乙"})]}
                       {:type "orderedList" :content [(item {:type "fact" :attrs {:key "saving-rate.site-c"}}
                                                            {:type "cite" :attrs {:block "ignored"}})]}]}]
    (db/apply-changes! c {:origin :manual :label "清單"
                          :changes {:nodes {:upsert [(assoc (dissoc p :node/version) :node/prose doc)]}}})
    (is (= #{"saving-rate.site-a" "saving-rate.site-c"}
           (set (d/q '[:find [?k ...] :in $ ?id
                       :where [?n :node/id ?id] [?n :node/facts ?f] [?f :fact/key ?k]]
                     (d/db c) (:node/id p))))
        "清單中的事實引用也寫入 :node/facts")
    (is (= doc (:node/prose (some #(when (= (:node/id p) (:node/id %)) %) (:nodes (db/load-state (d/db c)))))))))

(deftest target-chars-test
  (let [[_ c loaded] (seeded)
        state (client-state loaded)
        para (some #(when (= "節能效益估算" (:node/title %)) (:node/id %)) (:nodes loaded))
        target #(:node/target-chars (some (fn [n] (when (= para (:node/id n)) n)) (:nodes (db/load-state (d/db c)))))
        set400 (ui/set-target-chars state para "400")]
    (is (nil? (target)))
    (testing "寫入並讀回"
      (db/apply-changes! c {:label "設定目標字數" :changes (sync/diff state set400)})
      (is (= 400 (target))))
    (testing "改值再清除"
      (db/apply-changes! c {:label "改" :changes (sync/diff set400 (ui/set-target-chars set400 para "800"))})
      (is (= 800 (target)))
      (db/apply-changes! c {:label "清除" :changes (sync/diff (ui/set-target-chars set400 para "800") state)})
      (is (nil? (target)) "清除後欄位移除"))
    (testing "復原：清除復原回 800、再復原回 400、再復原回沒有"
      (db/undo! c)
      (is (= 800 (target)))
      (db/undo! c)
      (is (= 400 (target)))
      (db/undo! c)
      (is (nil? (target))))))

(deftest source-fields-test
  (let [[_ c loaded] (seeded)
        state (client-state loaded)
        table (some #(when (= "各場域量測結果" (:block/title %)) %) (:blocks loaded))
        tid (:block/id table)
        blocks #(by :block/id (:blocks (db/load-state (d/db c))))
        fact #(get (by :fact/key (:facts (db/load-state (d/db c)))) "saving-rate.site-b")]
    (testing "舊資料：:candidate 讀成 :confirmed，:block/ai-split 不讀出"
      (let [old-id (random-uuid)]
        (d/transact! c [{:block/id old-id :block/kind :para :block/status :candidate :block/ai-split true
                         :block/title "舊素材" :block/text "舊內容"}])
        (let [b (get (blocks) old-id)]
          (is (= :confirmed (:block/status b)))
          (is (not (contains? b :block/ai-split))))))
    (testing "素材的用途說明 :block/note 寫入、讀回、清除、復原"
      (let [with-note (update (get (blocks) tid) :block/note (constantly "看各場域節電率"))
            s1 (assoc state :blocks (mapv #(if (= tid (:block/id %)) with-note %) (:blocks (db/load-state (d/db c)))))]
        (db/apply-changes! c {:label "加用途說明" :changes (sync/diff (client-state (db/load-state (d/db c))) s1)})
        (is (= "看各場域節電率" (:block/note (get (blocks) tid))))
        (db/apply-changes! c {:label "清除"
                              :changes {:blocks {:upsert [(dissoc (get (blocks) tid) :block/note)]}}})
        (is (nil? (:block/note (get (blocks) tid))))
        (db/undo! c)
        (is (= "看各場域節電率" (:block/note (get (blocks) tid))))))
    (testing "事實指向表格儲存格：列（索引）與欄寫入、讀回、修改、清除、復原"
      (is (nil? (:fact/row (fact))))
      (let [f (fact)
            bound (assoc f :fact/block tid :fact/row 1 :fact/column "節電率")]
        (db/apply-changes! c {:label "綁定儲存格" :changes {:facts {:upsert [bound]}}})
        (is (= [tid 1 "節電率"] ((juxt :fact/block :fact/row :fact/column) (fact))))
        (db/apply-changes! c {:label "改列" :changes {:facts {:upsert [(assoc bound :fact/row 0)]}}})
        (is (= 0 (:fact/row (fact))) "列索引 0 也要存下來")
        (db/apply-changes! c {:label "解除" :changes {:facts {:upsert [(dissoc bound :fact/row :fact/column)]}}})
        (is (nil? (:fact/row (fact))))
        (is (nil? (:fact/column (fact))))
        (testing "復原一步步回到先前"
          (db/undo! c)
          (is (= [0 "節電率"] ((juxt :fact/row :fact/column) (fact))))
          (db/undo! c)
          (is (= [1 "節電率"] ((juxt :fact/row :fact/column) (fact))))
          (db/undo! c)
          (is (nil? (:fact/row (fact)))))))
    (testing "捨棄素材可復原"
      (let [b (get (blocks) tid)]
        (db/apply-changes! c {:label "捨棄" :changes {:blocks {:upsert [(assoc b :block/status :discarded)]}}})
        (is (= :discarded (:block/status (get (blocks) tid))))
        (db/undo! c)
        (is (= :confirmed (:block/status (get (blocks) tid))))))))
