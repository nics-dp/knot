(ns knot.history-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.history :as history]))

(def ^:private t0 1000000000000)

(defn- at [minutes] (let [ms (+ t0 (* minutes 60 1000))]
    #?(:clj (java.util.Date. ^long ms) :cljs (js/Date. ms))))

(defn- cs
  "測試用修改組；id 用整數代替 uuid 即可。"
  [id minutes node & {:keys [label by origin undone? relations]
                      :or {label "修改" by :user origin :manual}}]
  (cond-> {:changeset/id id :at (at minutes) :label label :by by :origin origin
           :undone? (boolean undone?) :relations (or relations [])}
    node (assoc :node node)))

(def base {:node/id :a :node/kind :paragraph :node/parent :p :node/order-key "M"
           :node/title "原標題" :node/draft "原意圖" :node/prose "原文"
           :node/status :none :node/citations [] :node/version 1})

(defn- change [& kvs] {:before base :after (apply assoc base kvs)})

(deftest create-import-delete-test
  (let [[e] (history/entries [(cs 1 0 {:before nil :after base} :origin :manual :label "新增")])]
    (is (= :create (:kind e)))
    (is (= #{} (:fields e)))
    (is (= [1] (:changeset-ids e)))
    (is (= 1 (:merged e))))
  (let [[e] (history/entries [(cs 1 0 {:before nil :after base} :origin :import :label "匯入"
                                  :relations [{:before nil :after {:relation/from :a :relation/to :b}}])])]
    (is (= :import (:kind e)))
    (is (= #{} (:fields e)) "匯入連帶的關係不列"))
  (let [[e] (history/entries [(cs 1 0 {:before base :after nil} :label "刪除")])]
    (is (= :delete (:kind e)))
    (is (= #{} (:fields e)))))

(deftest field-classification-test
  (let [one (fn [node] (first (history/entries [(cs 1 0 node)])))]
    (testing "名稱與意圖附前後文字"
      (let [e (one (change :node/title "新標題" :node/draft "新意圖"))]
        (is (= #{:name :intent} (:fields e)))
        (is (= {:name {:before "原標題" :after "新標題"}
                :intent {:before "原意圖" :after "新意圖"}}
               (:text-changes e)))))
    (testing "標題卡片用 :node/name"
      (let [h {:node/id :h :node/name "舊章名"}
            e (first (history/entries [(cs 1 0 {:before h :after (assoc h :node/name "新章名")})]))]
        (is (= {:name {:before "舊章名" :after "新章名"}} (:text-changes e)))))
    (testing "本文、依據、位置、狀態、其他設定"
      (is (= #{:prose} (:fields (one (change :node/prose "新文")))))
      (is (= #{:basis} (:fields (one (change :node/citations [{:block :x}])))))
      (is (= #{:position} (:fields (one (change :node/order-key "N")))))
      (is (= #{:position} (:fields (one (change :node/parent :q)))))
      (is (= #{:status} (:fields (one (change :node/status :ai)))))
      (is (= #{:settings} (:fields (one (change :node/label "圖 1")))))
      (testing "表格內容（write_table）獨立一類"
        (is (= #{:table} (:fields (one (change :node/table-content {:columns ["年"] :rows [["2025"]]})))))))
    (testing "只改版本號不列；沒有變動的欄位不列"
      (is (empty? (history/entries [(cs 1 0 (change :node/version 2))])))
      (is (= #{:prose} (:fields (one (change :node/prose "新文" :node/title "原標題"))))))
    (testing "關係變動"
      (let [rel {:relation/from :a :relation/to :b}
            e (first (history/entries [(cs 1 0 nil :relations [{:before nil :after rel}])]))]
        (is (= :change (:kind e)))
        (is (= #{:relations} (:fields e)))
        (is (= #{:relations} (:fields (first (history/entries
                                              [(cs 2 0 nil :relations [{:before rel :after nil}])])))))))))

(deftest undo-test
  (let [es (history/entries [(cs 1 0 (change :node/prose "新文") :undone? true)
                             (cs 2 1 {:before (:after (change :node/prose "新文")) :after base}
                                 :origin :undo :label "復原：修改")])]
    (is (= [:change :undo] (mapv :kind es)))
    (is (= [true false] (mapv :undone? es)))
    (is (= #{:prose} (:fields (second es))))))

(deftest merge-typing-test
  (let [typing (fn [id m before after & opts]
                 (apply cs id m {:before (assoc base :node/draft before) :after (assoc base :node/draft after)}
                        :label "編輯文字" opts))]
    (testing "連續編輯文字合併，取最早 before 與最晚 after"
      (let [es (history/entries [(typing 1 0 "a" "b") (typing 2 5 "b" "c") (typing 3 14 "c" "d")])]
        (is (= 1 (count es)))
        (is (= [1 2 3] (:changeset-ids (first es))))
        (is (= 3 (:merged (first es))))
        (is (= (at 14) (:at (first es))))
        (is (= {:intent {:before "a" :after "d"}} (:text-changes (first es))))))
    (testing "輸入順序不影響"
      (is (= [1 2] (:changeset-ids (first (history/entries [(typing 2 5 "b" "c") (typing 1 0 "a" "b")]))))))
    (testing "相隔超過 10 分鐘不合併，剛好 10 分鐘合併"
      (is (= 2 (count (history/entries [(typing 1 0 "a" "b") (typing 2 11 "b" "c")]))))
      (is (= 1 (count (history/entries [(typing 1 0 "a" "b") (typing 2 10 "b" "c")])))))
    (testing "中間夾別的修改組不合併"
      (is (= 3 (count (history/entries
                       [(typing 1 0 "a" "b")
                        (cs 2 1 (change :node/prose "新文") :label "重新生成")
                        (typing 3 2 "b" "c")])))))
    (testing "by 不同不合併"
      (is (= 2 (count (history/entries [(typing 1 0 "a" "b") (typing 2 1 "b" "c" :by :agent)])))))
    (testing "合併的項目任一個被復原就算已復原"
      (is (true? (:undone? (first (history/entries [(typing 1 0 "a" "b" :undone? true) (typing 2 1 "b" "c")]))))))
    (testing "打字後又改回原樣：沒有欄位變動，不列"
      (is (empty? (history/entries [(typing 1 0 "a" "b") (typing 2 1 "b" "a")]))))))

(deftest prose-change-test
  (let [typing (fn [id m before after]
                 (cs id m {:before (assoc base :node/prose before) :after (assoc base :node/prose after)}
                     :label "編輯文字" :by :agent))
        revs [(typing 1 0 "a" "b") (typing 2 3 "b" "c")
              (cs 3 60 (change :node/draft "新意圖"))]]
    (testing "合併項目取最早 before、最晚 after，任一 id 都找得到"
      (is (= {:prose? true :before "a" :after "c" :at (at 3) :label "編輯文字" :by :agent}
             (history/prose-change revs 1)
             (history/prose-change revs 2))))
    (testing "沒改本文時標明，找不到為 nil"
      (is (= {:prose? false :at (at 60) :label "修改" :by :user} (history/prose-change revs 3)))
      (is (nil? (history/prose-change revs 99))))))

(deftest timeline-test
  (let [thread (fn [id m] {:note {:note/id id :note/at (at m)} :replies []})
        change (fn [id m] {:changeset-ids [id] :at (at m) :label "x"})
        items (history/timeline [(thread "n1" 5) (thread "n2" 20)]
                                [(change 1 0) (change 2 10) (change 3 20)])]
    (testing "依時間穿插，舊到新"
      (is (= [[:change 1] [:thread "n1"] [:change 2] [:change 3] [:thread "n2"]]
             (map (fn [{:keys [type thread entry]}]
                    [type (if (= :thread type) (get-in thread [:note :note/id]) (first (:changeset-ids entry)))])
                  items))))
    (testing "同一時間時修改在前"
      (is (= [:change :thread] (map :type (history/timeline [(thread "n" 3)] [(change 1 3)])))))
    (testing "沒有歷程時只有留言串，反之亦然"
      (is (= [:thread] (map :type (history/timeline [(thread "n" 3)] nil))))
      (is (= [:change] (map :type (history/timeline [] [(change 1 3)])))))
    (testing "時間也可以是毫秒數"
      (is (= [:change :thread]
             (map :type (history/timeline [{:note {:note/at 2}}] [{:at 1 :changeset-ids [1]}])))))))

(defn- grp [c group-id name] (assoc c :group name :group-id group-id))

(deftest group-merge-test
  (let [s1 (assoc base :node/draft "一")
        s2 (assoc s1 :node/title "新名")
        s3 (assoc s2 :node/prose "新文")
        revs [(grp (cs 1 0 {:before base :after s1} :by :agent :label "改意圖") :g1 "依審查調整")
              (grp (cs 2 5 {:before s1 :after s2} :by :agent :label "改名") :g1 "依審查調整")
              (grp (cs 3 6 {:before s2 :after s3} :by :agent :label "文") :g1 "依審查調整")]
        es (history/entries revs)]
    (testing "同 group-id 且相鄰合併成一筆，label 為組名"
      (is (= 1 (count es)))
      (let [e (first es)]
        (is (= "依審查調整" (:label e)))
        (is (= 3 (:merged e)))
        (is (= [1 2 3] (:changeset-ids e)))
        (is (= #{:intent :name :prose} (:fields e)) "取各修改組的聯集")
        (is (= {:before "原意圖" :after "一"} (get-in e [:text-changes :intent])) "文字取最早 before、最晚 after")))
    (testing "prose-change 取最早 before、最晚 after"
      (let [c (history/prose-change revs 2)]
        (is (= "依審查調整" (:label c)))
        (is (= "原文" (:before c)))
        (is (= "新文" (:after c)))))
    (testing "只有一個修改組的組也用組名"
      (is (= "組" (:label (first (history/entries [(grp (cs 1 0 (change :node/draft "一") :by :agent) :g "組")]))))))
    (testing "被復原的成員讓整筆算已復原"
      (is (true? (:undone? (first (history/entries (map #(assoc % :undone? true) revs)))))))
    (testing "不同 group-id、或中間隔了別人的修改就不合併"
      (is (= 2 (count (history/entries [(grp (cs 1 0 (change :node/draft "一") :by :agent) :g1 "組")
                                        (grp (cs 2 1 (change :node/draft "二") :by :agent) :g2 "組")]))))
      (is (= 3 (count (history/entries [(grp (cs 1 0 (change :node/draft "一") :by :agent) :g1 "組")
                                        (cs 2 1 (change :node/draft "二"))
                                        (grp (cs 3 2 (change :node/draft "三") :by :agent) :g1 "組")])))))
    (testing "沒有 group 的行為不變"
      (is (= "修改" (:label (first (history/entries [(cs 1 0 (change :node/draft "一"))]))))))))
