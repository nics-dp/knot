(ns knot.notes-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [knot.db :as db]
            [knot.notes :as notes]
            [knot.seed :as seed]
            [knot.ui :as ui]))

(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [t]
    (let [dir (str (java.nio.file.Files/createTempDirectory "knot-notes" (make-array java.nio.file.attribute.FileAttribute 0)))
          conn (db/open-report dir)]
      (try
        (seed/seed! conn (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))
        (binding [*conn* conn] (t))
        (finally (db/close-report conn))))))

(defn- card [title]
  (some #(when (= title (:node/title %)) (:node/id %)) (:nodes (db/load-state (d/db *conn*)))))

(defn- user! [m] (notes/add-note! *conn* (merge {:author :user} m)))
(defn- agent! [m] (notes/add-note! *conn* (merge {:author :agent} m)))
(defn- db* [] (d/db *conn*))
(defn- ids [ns] (mapv :note/id ns))

(deftest add-note-test
  (let [c (card "主要成果")
        n (user! {:node c :text "這段要補數據"})]
    (is (= {:note/node c :note/author :user :note/text "這段要補數據"}
           (select-keys n [:note/node :note/author :note/text])))
    (is (uuid? (:note/id n)))
    (is (inst? (:note/at n)))
    (is (not (contains? n :note/todo)))
    (is (= [n] (notes/list-notes (db*))))
    (is (= [n] (:notes (db/load-state (db*)))) "load-state 帶留言")
    (testing "整份報告的留言"
      (let [r (user! {:node nil :text "整體意見"})]
        (is (not (contains? r :note/node)))))
    (testing "驗證"
      (is (= {:error "留言內容不能是空白"} (user! {:node c :text "  "})))
      (is (= {:error "留言內容不能是空白"} (user! {:node c :text nil})))
      (is (= {:error "找不到卡片"} (user! {:node (random-uuid) :text "x"})))
      (is (= {:error "找不到要回覆的留言"} (user! {:node c :text "x" :reply-to (random-uuid)}))))))

(deftest reply-test
  (let [c (card "主要成果")
        q (agent! {:node c :text "數據來源是哪份？"})
        r (user! {:reply-to (:note/id q) :text "見附件"})]
    (is (= c (:note/node r)) "回覆沿用上層留言的卡片")
    (is (= (:note/id q) (:note/reply-to r)))
    (is (= {:error "回覆的卡片必須與被回覆的留言相同"}
           (user! {:node (card "計畫目標") :reply-to (:note/id q) :text "x"})))
    (is (not (:error (user! {:node c :reply-to (:note/id q) :text "同一張卡片可以給"}))))))

(deftest todo-rules-test
  (let [c (card "主要成果")]
    (is (= {:error "只有使用者能建立待辦"} (agent! {:node c :text "x" :todo true})))
    (let [t (user! {:node c :text "請補充" :todo true})]
      (is (= {:note/todo true :note/state :open} (select-keys t [:note/todo :note/state]))))
    (testing "set-todo!"
      (let [n (user! {:node c :text "一般留言"})
            t (notes/set-todo! *conn* (:note/id n) true)]
        (is (= {:note/todo true :note/state :open} (select-keys t [:note/todo :note/state])))
        (let [u (notes/set-todo! *conn* (:note/id n) false)]
          (is (= n u) "取消後回到原本的留言"))
        (is (= {:error "只有使用者自己的留言能標為待辦"}
               (notes/set-todo! *conn* (:note/id (agent! {:node c :text "q"})) true)))
        (is (= {:error "找不到留言"} (notes/set-todo! *conn* (random-uuid) true)))))))

(deftest set-state-test
  (let [c (card "主要成果")
        t (user! {:node c :text "請補充" :todo true})
        id (:note/id t)
        cs (random-uuid)]
    (is (= {:error "這則留言不是待辦"}
           (notes/set-state! *conn* (:note/id (user! {:node c :text "x"})) :done {})))
    (let [done (notes/set-state! *conn* id :done {:changeset cs})]
      (is (= :done (:note/state done)))
      (is (inst? (:note/done-at done)))
      (is (= cs (:note/changeset done)))
      (is (empty? (notes/open-todos (db*)))))
    (let [open (notes/set-state! *conn* id :open {})]
      (is (= :open (:note/state open)))
      (is (not-any? #(contains? open %) [:note/done-at :note/changeset]))
      (is (= [id] (ids (notes/open-todos (db*))))))
    (testing "不指定修改組"
      (let [done (notes/set-state! *conn* id :done {})]
        (is (inst? (:note/done-at done)))
        (is (not (contains? done :note/changeset)))))
    (is (= {:error "待辦狀態只能是 open 或 done"} (notes/set-state! *conn* id :x {})))))

(deftest delete-note-test
  (let [c (card "主要成果")
        q (agent! {:node c :text "問題"})
        r1 (user! {:reply-to (:note/id q) :text "回答"})
        r2 (agent! {:reply-to (:note/id r1) :text "追問"})
        other (user! {:node c :text "另一則"})]
    (is (= {:error "只能刪除自己的留言"} (notes/delete-note! *conn* (:note/id q) :user)))
    (is (= {:error "找不到留言"} (notes/delete-note! *conn* (random-uuid) :user)))
    (is (= 4 (count (notes/list-notes (db*)))))
    (let [res (notes/delete-note! *conn* (:note/id q) :agent)]
      (is (= #{(:note/id q) (:note/id r1) (:note/id r2)} (set (:deleted res)))))
    (is (= [(:note/id other)] (ids (notes/list-notes (db*)))) "回覆一併刪除")))

(deftest delete-knot-note-test
  (let [k (first (notes/record-change!
                  *conn*
                  (ui/from-server (db/load-state (d/db *conn*)))
                  (-> (ui/from-server (db/load-state (d/db *conn*)))
                      (assoc-in [:nodes (card "各場域結果比較") :node/draft] "改過"))))]
    (is (= :knot (:note/author k)))
    (is (= {:error "只能刪除自己的留言"} (notes/delete-note! *conn* (:note/id k) :agent)))
    (is (= {:deleted [(:note/id k)]} (notes/delete-note! *conn* (:note/id k) :user)) "使用者可以刪除 Knot 待辦")))

(deftest record-user-change-test
  (let [load #(ui/from-server (db/load-state (d/db *conn*)))
        p312 (card "各場域結果比較")
        before (load)
        after (assoc-in before [:nodes p312 :node/prose] {:type "doc" :content []})
        ns (notes/record-change! *conn* before after)]
    (is (= 2 (count ns)) "主要成果與第三章引言還沒有本文，不留")
    (is (every? #(= :knot (:note/author %)) (notes/list-notes (db*))))
    (is (= {(card "節能效益估算") 1 (card "異常狀況說明") 1} (notes/todo-counts (db*))))
    (testing "同一觸發卡片的未完成待辦不重複，完成後會再留"
      (is (empty? (notes/record-change! *conn* before after)))
      (notes/set-state! *conn* (:note/id (first ns)) :done {})
      (is (= 1 (count (notes/record-change! *conn* before after)))))
    (testing "標題修改不觸發"
      (is (empty? (notes/record-change! *conn* before (assoc-in before [:nodes (:node/id (first (filter #(= :heading (:node/kind %)) (vals (:nodes before))))) :node/name] "新名")))))))

(deftest deleted-card-hides-notes-test
  (let [c (card "主要成果")
        t (user! {:node c :text "請補充" :todo true})
        _ (user! {:node nil :text "整體意見" :todo true})]
    (is (= 2 (count (notes/open-todos (db*)))))
    (is (= {c 1} (notes/todo-counts (db*))) "整份報告的待辦不列入")
    (db/apply-changes! *conn* {:changes {:nodes {:delete [c]}} :label "刪除"})
    (is (= 2 (count (notes/list-notes (db*)))) "留言仍在資料庫")
    (is (= 1 (count (notes/open-todos (db*)))) "卡片已刪除，留言隱藏")
    (is (= {} (notes/todo-counts (db*))))
    (db/undo! *conn*)
    (is (= #{(:note/id t)} (set (map :note/id (filter :note/node (notes/open-todos (db*))))))
        "復原卡片後留言回來")
    (is (= {c 1} (notes/todo-counts (db*))))))

(deftest agent-questions-test
  (let [c (card "主要成果")
        q1 (agent! {:node c :text "問題一"})
        q2 (agent! {:node nil :text "問題二"})
        _ (user! {:reply-to (:note/id q2) :text "回答二"})]
    (is (= [(:note/id q1)] (ids (notes/agent-questions (db*)))) "有回覆的不算")
    (is (= [(:note/id q2)] (ids (notes/agent-threads (db*)))))
    (is (= ["回答二"] (map :note/text (notes/thread (db*) (:note/id q2)))))))

(deftest latest-agent-changeset-test
  (let [c (card "主要成果")
        t (user! {:node c :text "請補充" :todo true})
        before (:note/at t)]
    (is (nil? (notes/latest-agent-changeset (db*) c before)))
    (Thread/sleep 5)
    (let [cs (:changeset/id (db/apply-changes!
                             *conn* {:by :agent :label "改"
                                     :changes {:nodes {:upsert [(-> (some #(when (= c (:node/id %)) %) (:nodes (db/load-state (db*))))
                                                                    (assoc :node/draft "新意圖"))]}}}))]
      (is (= cs (notes/latest-agent-changeset (db*) c before)))
      (is (nil? (notes/latest-agent-changeset (db*) (card "計畫目標") before)))
      (is (nil? (notes/latest-agent-changeset (db*) nil before))))))

(deftest knot-todo-merge-test
  (let [load #(ui/from-server (db/load-state (d/db *conn*)))
        p312 (card "各場域結果比較")
        p311 (card "測試方法概述")
        p313 (card "異常狀況說明")
        b0 (load)
        a1 (assoc-in b0 [:nodes p312 :node/draft] "改一")
        n1 (notes/record-change! *conn* b0 a1)
        target #(first (filter (fn [n] (= p313 (:note/node n))) (notes/list-notes (db*))))]
    (is (= 2 (count n1)))
    (is (= [p312] (map :id (:note/triggers (target)))))
    (testing "同一目標再有新的觸發：附加到同一則，不新增"
      (let [b1 (load)
            a2 (-> b1
                   (assoc-in [:nodes p311 :node/prose] {:type "doc" :content [{:type "paragraph"}]})
                   (update :relations conj {:relation/from p313 :relation/to p311 :relation/type :basis :relation/remote true}))
            n2 (notes/record-change! *conn* b1 a2)
            t (target)]
        (is (= [p313] (map :note/node (filter :merged? n2))) "p312 因遞進承接另有新的一則，p313 為附加")
        (is (= [p312 p311] (map :id (:note/triggers t))))
        (is (= 1 (count (filter #(= p313 (:note/node %)) (notes/list-notes (db*))))))
        (is (= (:note/id (first (filter :merged? n2))) (:note/id t)))
        (is (re-find #"改了，本段因果承接它們、以它們為依據" (:note/text t)))
        (is (inst? (:note/updated-at (first (filter #(= (:note/id t) (:note/id %)) (d/q '[:find [(pull ?e [*]) ...] :where [?e :note/id]] (db*)))))))))
    (testing "舊的單一 :note/trigger 讀成一個元素的 triggers"
      (let [id (random-uuid)]
        (d/transact! *conn* [{:note/id id :note/author :knot :note/todo true :note/state :open :note/at (java.util.Date.)
                              :note/node p312 :note/trigger p311 :note/dependency :argument}])
        (is (= [{:id p311 :dependency :argument}]
               (:note/triggers (first (filter #(= id (:note/id %)) (notes/list-notes (db*)))))))))))
