(ns knot.tree-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.tree :as tree]))

(defn- h [id name & children] (cond-> {:node/id id :node/kind :heading :node/name name}
                                (seq children) (assoc :children (vec children))))
(defn- p [id & children] (cond-> {:node/id id :node/kind :paragraph :node/title id}
                           (seq children) (assoc :children (vec children))))
(defn- intro [id] {:node/id id :node/kind :intro})
(defn- tbl [id] {:node/id id :node/kind :table :node/label (str "tab:" id)})

;; 第一章 A
;;   引言 iA
;;   1.1 A1：段落 a1 a2 a3（a2 底下有表 t1）
;;   1.2 A2：1.2.1 A21 → 1.2.1.1 A211：段落 x1
;; 第二章 B：段落 b1 b2
(def nodes
  (tree/from-nested
   [(h "A" "甲"
       (intro "iA")
       (h "A1" "甲一" (p "a1") (p "a2" (tbl "t1")) (p "a3"))
       (h "A2" "甲二" (h "A21" "甲二一" (h "A211" "甲二一一" (p "x1")))))
    (h "B" "乙" (p "b1") (p "b2"))]))

(deftest structure-test
  (is (= ["A" "B"] (tree/child-ids nodes nil)))
  (is (= ["iA" "A1" "A2"] (tree/child-ids nodes "A")))
  (is (= 4 (tree/depth nodes "A211")))
  (is (= ["A1" "A"] (tree/ancestor-ids nodes "a2")))
  (is (= ["a1" "a2" "t1" "a3"] (tree/descendant-ids nodes "A1")))
  (is (= ["A" "iA" "A1" "a1" "a2" "t1" "a3" "A2" "A21" "A211" "x1" "B" "b1" "b2"]
         (tree/document-order nodes)))
  (is (= 4 (tree/subtree-heading-height nodes "A")))
  (is (= "iA" (tree/intro-id nodes "A")))
  (is (= "a1" (tree/prev-paragraph nodes "a2")))
  (is (nil? (tree/prev-paragraph nodes "A1"))))

(deftest numbering-test
  (let [nums (tree/numbering nodes)]
    (is (= {"A" "1" "A1" "1.1" "A2" "1.2" "A21" "1.2.1" "A211" "1.2.1.1" "B" "2"} nums))
    (is (= "第一章 甲" (tree/display-title nodes nums "A")))
    (is (= "1.2.1.1 甲二一一" (tree/display-title nodes nums "A211")))
    (is (= "引言" (tree/display-title nodes nums "iA")))
    (is (= "a1" (tree/display-title nodes nums "a1")))))

(deftest position-label-test
  (let [nums (tree/numbering nodes)]
    (is (= "1.1" (tree/position-label nodes nums "a1")) "段落的位置是所屬標題的編號")
    (is (= "1.1" (tree/position-label nodes nums "t1")) "表格往上找最近的標題")
    (is (= "第一章" (tree/position-label nodes nums "iA")) "章底下直接的卡片寫第幾章")
    (is (nil? (tree/position-label nodes nums "A")) "最上層的章沒有位置")))

(deftest chinese-numeral-test
  (is (= ["一" "九" "十" "十一" "二十" "二十三"]
         (map tree/chinese-numeral [1 9 10 11 20 23]))))

(deftest accepts-test
  (testing "最上層只能放標題"
    (is (tree/accepts? nodes nil "A1"))
    (is (not (tree/accepts? nodes nil "a1"))))
  (testing "段落與下層標題可以並存（順序另見 order-ok?）"
    (is (tree/accepts? nodes "B" "a1"))
    (is (tree/accepts? nodes "A" "a1"))
    (is (tree/accepts? nodes "B" "A21")))
  (testing "引言最多一張"
    (is (not (tree/accepts? (assoc nodes "iX" {:node/id "iX" :node/kind :intro}) "A" "iX")))
    (is (tree/accepts? (assoc nodes "iX" {:node/id "iX" :node/kind :intro}) "B" "iX")))
  (testing "表格與圖只能掛在段落底下"
    (is (tree/accepts? nodes "a1" "t1"))
    (is (not (tree/accepts? nodes "A1" "t1"))))
  (testing "標題連同子樹不得超過四層"
    (is (not (tree/accepts? nodes "A21" "A21")))
    (is (tree/accepts? nodes "A" "A21"))
    (is (not (tree/accepts? (assoc nodes "Z" {:node/id "Z" :node/kind :heading :node/parent "A2" :node/order-key "z"})
                            "Z" "A21"))))
  (testing "不能放進自己的子孫"
    (is (not (tree/accepts? nodes "A211" "A2")))))

(deftest placement-test
  (testing "同層插入前後"
    (is (= {:parent "A1" :index 0} (tree/placement nodes "a3" "a1" :before)))
    ;; index 以「不含被移動卡片」的兄弟計算：[a2 a3] 中 a2 之後是 1
    (is (= {:parent "A1" :index 1} (tree/placement nodes "a1" "a2" :after))))
  (testing "引言固定在最前面"
    (is (nil? (tree/placement nodes "A2" "iA" :before)))
    (is (= {:parent "A" :index 1} (tree/placement nodes "A2" "iA" :after))))
  (testing "移入放在最後"
    (is (= {:parent "B" :index 2} (tree/placement nodes "a1" "B" :into))))
  (testing "不能拖到自己或子孫"
    (is (nil? (tree/placement nodes "A" "A" :into)))
    (is (nil? (tree/placement nodes "A2" "A211" :into)))))

;; 第一章 M：引言 iM、段落 m1 m2（m2 底下有表 mt）、1.1 M1：段落 q1、1.2 M2
(def mixed
  (tree/from-nested
   [(h "M" "混"
       (intro "iM") (p "m1") (p "m2" (tbl "mt"))
       (h "M1" "混一" (p "q1"))
       (h "M2" "混二"))
    (h "N" "乙" (p "n1"))]))

(deftest mixed-order-test
  (is (= ["M" "iM" "m1" "m2" "mt" "M1" "q1" "M2" "N" "n1"] (tree/document-order mixed))
      "全文順序：標題、引言、段落、下層標題")
  (is (= {"M" "1" "M1" "1.1" "M2" "1.2" "N" "2"} (tree/numbering mixed)))
  (is (= "m1" (tree/prev-paragraph mixed "m2")))
  (is (nil? (tree/prev-paragraph mixed "M1")) "下層標題前面的段落不算它的前一段")
  (is (= "M1" (tree/next-sibling mixed "m2"))))

(deftest order-ok-test
  (testing "段落排在下層標題之後不行"
    (is (tree/order-ok? mixed "M" "n1" 3) "m1 m2 之後、M1 之前")
    (is (not (tree/order-ok? mixed "M" "n1" 4)) "M1 之後")
    (is (not (tree/order-ok? mixed "M" "n1" 0)) "引言之前"))
  (testing "標題排在段落之前不行"
    (is (tree/order-ok? mixed "M" "N" 4) "最後")
    (is (tree/order-ok? mixed "M" "N" 3) "段落之後、第一個下層標題之前")
    (is (not (tree/order-ok? mixed "M" "N" 2)) "m1 之後、m2 之前")
    (is (not (tree/order-ok? mixed "M" "N" 1)) "引言之後、段落之前")))

(deftest mixed-placement-test
  (testing "before / after"
    (is (= {:parent "M" :index 2} (tree/placement mixed "n1" "m2" :before)))
    (is (= {:parent "M" :index 3} (tree/placement mixed "n1" "m2" :after)))
    (is (nil? (tree/placement mixed "n1" "M1" :after)) "段落不能排在下層標題之後")
    (is (= {:parent "M" :index 3} (tree/placement mixed "n1" "M1" :before)))
    (is (nil? (tree/placement mixed "N" "m1" :before)) "標題不能排在段落之前")
    (is (nil? (tree/placement mixed "N" "iM" :after)) "引言之後是段落，標題不能插在中間")
    (is (= {:parent "M" :index 5} (tree/placement mixed "N" "M2" :after)))
    (is (= {:parent "M" :index 3} (tree/placement mixed "N" "m2" :after))))
  (testing "into：段落放在最後一個段落之後，標題放最後"
    (is (= {:parent "M" :index 3} (tree/placement mixed "n1" "M" :into)))
    (is (= {:parent "M" :index 5} (tree/placement mixed "N" "M" :into))))
  (testing "標題內的段落移到同層其他段落之間"
    (is (= {:parent "M" :index 1} (tree/placement mixed "m2" "m1" :before)))
    (is (nil? (tree/placement mixed "m1" "M1" :after)))))

(deftest mixed-sibling-placement-test
  (is (nil? (tree/sibling-placement mixed "m2" 1)) "段落不能下移到下層標題之後")
  (is (nil? (tree/sibling-placement mixed "M1" -1)) "下層標題不能上移到段落之前")
  (is (= {:parent "M" :index 4} (tree/sibling-placement mixed "M1" 1)) "下層標題之間可以交換")
  (is (= {:parent "M" :index 1} (tree/sibling-placement mixed "m2" -1))))

(deftest insert-key-test
  (let [pos (fn [kind]
              (let [k (tree/insert-key mixed "M" kind)
                    ns (assoc mixed "new" {:node/id "new" :node/kind kind :node/parent "M" :node/order-key k})]
                (.indexOf (tree/child-ids ns "M") "new")))]
    (is (= 0 (pos :intro)))
    (is (= 3 (pos :paragraph)) "最後一個段落之後、第一個下層標題之前")
    (is (= 5 (pos :heading)) "標題放最後")))

(deftest move-test
  (let [moved (tree/move nodes "a3" (tree/placement nodes "a3" "a1" :before))]
    (is (= ["a3" "a1" "a2"] (tree/child-ids moved "A1")))
    (testing "只改被移動卡片"
      (is (= (dissoc moved "a3") (dissoc nodes "a3")))))
  (let [moved (tree/move nodes "A21" (tree/placement nodes "A21" "A1" :after))]
    (testing "標題移動後層級隨新位置改變，編號重算"
      (is (= ["iA" "A1" "A21" "A2"] (tree/child-ids moved "A")))
      (is (= "1.2" (get (tree/numbering moved) "A21")))
      (is (= "1.2.1" (get (tree/numbering moved) "A211"))))))

(deftest sibling-placement-test
  (is (= {:parent "A1" :index 0} (tree/sibling-placement nodes "a2" -1)))
  (is (nil? (tree/sibling-placement nodes "a1" -1)))
  (is (nil? (tree/sibling-placement nodes "a3" 1)))
  (testing "不能移到引言前面"
    (is (nil? (tree/sibling-placement nodes "A1" -1)))))

(deftest relations-after-move-test
  (let [rels [{:relation/from "a1" :relation/to "a2" :relation/type :causal}
              {:relation/from "a2" :relation/to "a3" :relation/type :progression}
              {:relation/from "b1" :relation/to "b2" :relation/type :contrast}
              {:relation/from "b2" :relation/to "a2" :relation/type :summarizes}]
        {:keys [nodes relations]} (tree/move-with-relations {:nodes nodes :relations rels}
                                                            "a2" {:parent "B" :index 1})
        by-pair (into {} (map (juxt (juxt :relation/from :relation/to) identity)) relations)]
    (is (= ["b1" "a2" "b2"] (tree/child-ids nodes "B")))
    (testing "舊位置的下一段與新的前一段：待確認"
      (is (:relation/pending (by-pair ["a1" "a3"]))))
    (testing "被移動的段落與新的前一段：待確認"
      (is (:relation/pending (by-pair ["b1" "a2"]))))
    (testing "新位置的下一段與被移動的段落：待確認"
      (is (:relation/pending (by-pair ["a2" "b2"]))))
    (testing "舊的相鄰關係移除"
      (is (nil? (by-pair ["a1" "a2"])))
      (is (nil? (by-pair ["a2" "a3"])))
      (is (nil? (by-pair ["b1" "b2"]))))
    (testing "非相鄰的摘要關係保留"
      (is (= :summarizes (:relation/type (by-pair ["b2" "a2"])))))))

(deftest char-counts-test
  (let [doc (fn [s] {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text s}]}]})
        state {:facts {}
               :nodes (tree/from-nested
                       [{:node/id "A" :node/kind :heading :node/name "甲"
                         :children [{:node/id "iA" :node/kind :intro :node/prose (doc "引 言")}
                                    {:node/id "a1" :node/kind :paragraph :node/title "一" :node/prose (doc "一二三")}
                                    {:node/id "a2" :node/kind :paragraph :node/title "二"}
                                    {:node/id "A1" :node/kind :heading :node/name "甲一"
                                     :children [{:node/id "x1" :node/kind :paragraph :node/title "x" :node/prose (doc "四五")}
                                                {:node/id "t1" :node/kind :table :node/title "表"
                                                 :node/table-content {:columns ["欄"] :rows [["不算"]]}}]}]}
                        {:node/id "B" :node/kind :heading :node/name "乙"}])}
        counts (tree/char-counts state)]
    (testing "段落與引言為本文字數，沒有本文為 0"
      (is (= 2 (counts "iA")))
      (is (= 3 (counts "a1")))
      (is (= 0 (counts "a2"))))
    (testing "標題為底下所有段落與引言的合計（含下層標題）；沒有內容為 0"
      (is (= 2 (counts "A1")))
      (is (= 7 (counts "A")))
      (is (= 0 (counts "B"))))
    (testing "表格與圖沒有字數"
      (is (not (contains? counts "t1"))))
    (testing "單張卡片"
      (is (= 7 (tree/subtree-chars state "A")))
      (is (nil? (tree/subtree-chars state "t1"))))))

(deftest dependents-test
  (let [ids (fn [ds] (mapv (juxt :id :dependency :reason) (remove #(= "iA" (:id %)) ds)))
        with-intro (fn [ds] (mapv (juxt :id :dependency :reason) ds))
        rel (fn [from to type & {:as m}] (merge {:relation/from from :relation/to to :relation/type type} m))]
    (testing "摘要它的段落與上層引言（依文件順序）"
      (let [rels [(rel "b2" "a2" :summarizes)]]
        (is (= [["iA" :summary "是它的上層引言"] ["b2" :summary "摘要它"]]
               (with-intro (tree/dependents nodes rels "a2"))))))
    (testing "因果、遞進承接下一段；待確認也算"
      (is (= [["a2" :argument "因果承接"]]
             (ids (tree/dependents nodes [(rel "a1" "a2" :causal)] "a1")))) 
      (is (= [["a3" :argument "遞進承接"]]
             (ids (tree/dependents nodes [(rel "a2" "a3" :progression :relation/pending true)] "a2")))))
    (testing "方向：被承接的一方不依賴前一段"
      (is (empty? (ids (tree/dependents nodes [(rel "a1" "a2" :causal)] "a2")))))
    (testing "依據"
      (is (= [["b1" :argument "以它為依據"]]
             (ids (tree/dependents nodes [(rel "b1" "a3" :basis :relation/remote true)] "a3")))))
    (testing "呼應、對比、舉例不算"
      (is (empty? (ids (tree/dependents nodes [(rel "a1" "a2" :contrast) (rel "b1" "a3" :echo :relation/remote true)
                                               (rel "b2" "a3" :contrast :relation/remote true) (rel "a3" "b1" :example)]
                                        "a3"))))
      (is (= [["a2" :argument "因果承接"]]
             (ids (tree/dependents nodes [(rel "a1" "a2" :causal) (rel "a1" "b1" :echo :relation/remote true)] "a1")))))
    (testing "同一張卡片有多種原因時只留一個：摘要優先於論證"
      (is (= [["a2" :summary "摘要它"]]
             (ids (tree/dependents nodes [(rel "a1" "a2" :causal) (rel "a2" "a1" :summarizes)] "a1")))))
    (testing "上層引言：有引言卡片的標題才算"
      (is (empty? (tree/dependents nodes [] "b1")))
      (is (= [{:id "iA" :dependency :summary :reason "是它的上層引言"}] (tree/dependents nodes [] "a1")))
      (is (= [{:id "iA" :dependency :summary :reason "是它的上層引言"}] (tree/dependents nodes [] "x1")) "x1 的上層標題 A 有引言"))
    (testing "只有段落有依賴"
      (is (empty? (tree/dependents nodes [(rel "a1" "a2" :causal)] "A1")))
      (is (empty? (tree/dependents nodes [] "iA")))
      (is (empty? (tree/dependents nodes [] "t1"))))))

(defn- rel [from to type] {:relation/from from :relation/to to :relation/type type})

(deftest writing-order-test
  (testing "沒有關係：前一段先於本段，引言排在整章段落之後"
    (is (= {:batches [["a1" "x1" "b1"] ["a2" "b2"] ["a3"] ["iA"]] :cycle []}
           (tree/writing-order nodes []))
        "批內依文件順序；表格不出現"))
  (testing "依據：目標先於依據它的段落"
    (is (= [["a1" "x1" "b1"] ["a2" "b2"] ["a3"] ["iA"]]
           (:batches (tree/writing-order nodes [(rel "a1" "a3" :causal)]))))
    (let [{:keys [batches]} (tree/writing-order nodes [(rel "b1" "x1" :basis)])]
      (is (= [["a1" "x1"] ["a2" "b1"] ["a3" "b2"] ["iA"]] batches))))
  (testing "摘要：目標先於摘要它的段落"
    (let [{:keys [batches]} (tree/writing-order nodes [(rel "a1" "b2" :summarizes)])]
      (is (= [["x1" "b1"] ["b2"] ["a1"] ["a2"] ["a3"] ["iA"]] batches))))
  (testing "引言：節的引言先於章的引言"
    (let [n (tree/from-nested
             [(h "A" "甲" (intro "iA") (h "A1" "甲一" (intro "iA1") (p "a1")))])]
      (is (= [["a1"] ["iA1"] ["iA"]] (:batches (tree/writing-order n []))))))
  (testing "限定標題：範圍外不出現也不構成先後"
    (let [rels [(rel "b1" "a1" :basis)]]
      (is (= {:batches [["a1"] ["a2"] ["a3"]] :cycle []}
             (tree/writing-order nodes rels "A1")) "A1 的 a1 依據範圍外的 b1，忽略")
      (is (= [["a1" "x1"] ["a2"] ["a3"] ["iA"]] (:batches (tree/writing-order nodes [] "A"))))))
  (testing "環：排不進去的放在最後一批並列在 :cycle"
    (let [{:keys [batches cycle]} (tree/writing-order nodes [(rel "a1" "a2" :basis) (rel "a2" "a1" :basis)])]
      (is (= ["iA" "a1" "a2" "a3"] cycle) "環上的卡片與被它們擋住的卡片（a3、章的引言）")
      (is (= cycle (last batches)))
      (is (= [["x1" "b1"] ["b2"]] (butlast batches))))))


(deftest adjacent-paragraph-across-headings-test
  (testing "全文順序：跨標題、隔引言、隔表與圖都是相鄰"
    (is (= "a3" (tree/prev-paragraph nodes "x1")) "隔著 1.2、1.2.1、1.2.1.1 三層標題")
    (is (= "x1" (tree/prev-paragraph nodes "b1")) "隔著第二章標題")
    (is (= "a1" (tree/prev-paragraph nodes "a2")) "表 t1 在 a2 底下，不影響 a2 之後")
    (is (= "a2" (tree/prev-paragraph nodes "a3")) "隔著 a2 底下的表")
    (is (= "b1" (tree/next-paragraph nodes "x1")))
    (is (= "x1" (tree/next-paragraph nodes "a3")))
    (is (nil? (tree/next-paragraph nodes "b2")) "最後一段沒有後一段"))
  (testing "第一段沒有前一段；引言、標題、表不是段落"
    (is (nil? (tree/prev-paragraph nodes "a1")) "章的引言不參與")
    (is (nil? (tree/prev-paragraph nodes "iA")))
    (is (nil? (tree/next-paragraph nodes "A")))
    (is (nil? (tree/prev-paragraph nodes "t1"))))
  (testing "引言夾在中間也略過"
    (let [n (tree/from-nested [(h "A" "甲" (h "A1" "甲一" (p "a1")) (h "A2" "甲二" (intro "iA2") (p "a2")))])]
      (is (= "a1" (tree/prev-paragraph n "a2")))))
  (testing "隔圖"
    (let [n (tree/from-nested [(h "A" "甲" (p "a1" {:node/id "f1" :node/kind :figure}) (p "a2"))])]
      (is (= "a1" (tree/prev-paragraph n "a2")))))
  (testing "adjacent-pairs 一次算出全部"
    (let [{:keys [prev next]} (tree/adjacent-pairs nodes)]
      (is (= {"a2" "a1" "a3" "a2" "x1" "a3" "b1" "x1" "b2" "b1"} prev))
      (is (= {"a1" "a2" "a2" "a3" "a3" "x1" "x1" "b1" "b1" "b2"} next)))))

(deftest reconcile-adjacent-test
  (let [rels [(rel "a1" "a2" :causal) (rel "a2" "a3" :progression) (rel "a3" "x1" :contrast)
              (rel "x1" "b1" :example) (rel "b1" "b2" :causal)
              {:relation/from "b2" :relation/to "a1" :relation/type :summarizes :relation/remote true}]
        by-pair (fn [rs] (into {} (map (juxt (juxt :relation/from :relation/to) identity)) rs))
        reconcile (fn [nodes'] (by-pair (tree/reconcile-adjacent rels nodes nodes')))]
    (testing "沒有變動：關係不動"
      (is (= (by-pair rels) (reconcile nodes))))
    (testing "移動整個標題：邊界兩側的段落待確認，舊關係移除，標題內部的關係保留"
      ;; 把 A2（含 x1）移到 A1 之前：全文順序 a... 變成 x1 a1 a2 a3 b1
      (let [moved (tree/move nodes "A2" {:parent "A" :index 1})
            r (reconcile moved)]
        (is (= ["iA" "A2" "A1"] (tree/child-ids moved "A")))
        (is (:relation/pending (r ["x1" "a1"])) "x1 成為 a1 的前一段")
        (is (:relation/pending (r ["a3" "b1"])) "a3 成為 b1 的前一段")
        (is (nil? (r ["a3" "x1"])))
        (is (nil? (r ["x1" "b1"])))
        (is (= :causal (:relation/type (r ["a1" "a2"]))) "不受影響的關係保留")
        (is (not (:relation/pending (r ["a1" "a2"]))))
        (is (= :summarizes (:relation/type (r ["b2" "a1"]))) "不相鄰關係保留")))
    (testing "移動段落（原本已有關係時保留類型，標為待確認）"
      (let [moved (tree/move nodes "x1" {:parent "A1" :index 0})
            r (reconcile moved)]
        (is (= ["x1" "a1" "a2" "a3"] (tree/child-ids moved "A1")))
        (is (nil? (r ["a3" "x1"])))
        (is (nil? (r ["x1" "b1"])))
        (is (:relation/pending (r ["a3" "b1"])) "舊位置的下一段承接新的前一段")
        (is (:relation/pending (r ["x1" "a1"])))
        (is (nil? (:relation/type (r ["x1" "a1"]))) "原本沒有關係：未設定、待確認")
        (is (:relation/pending (r ["a3" "b1"])))))
    (testing "刪除標題：前後段落接起來，待確認"
      (let [gone #{"A2" "A21" "A211" "x1"}
            nodes' (apply dissoc nodes gone)
            rels' (vec (remove #(or (gone (:relation/from %)) (gone (:relation/to %))) rels))
            r (by-pair (tree/reconcile-adjacent rels' nodes nodes'))]
        (is (:relation/pending (r ["a3" "b1"])))
        (is (nil? (r ["x1" "b1"])))
        (is (= :causal (:relation/type (r ["a1" "a2"]))))))
    (testing "新增段落：後面的段落待確認，新段落本身沒有關係，舊的相鄰關係移除"
      (let [nodes' (assoc nodes "n1" {:node/id "n1" :node/kind :paragraph :node/parent "A1"
                                      :node/order-key (tree/key-after nodes "a1")})
            r (reconcile nodes')]
        (is (:relation/pending (r ["n1" "a2"])))
        (is (nil? (r ["a1" "a2"])))
        (is (nil? (r ["a1" "n1"])))
        (is (= :progression (:relation/type (r ["a2" "a3"]))))))))

(deftest writing-order-across-headings-test
  (testing "同一 parent 的前一段照常是先後；跨標題沒有已確認的關係時不連邊"
    (let [b (:batches (tree/writing-order nodes []))]
      (is (= ["a1" "x1" "b1"] (first b)) "x1 與 b1 各自是第一批，可並行")))
  (testing "跨標題已確認、有類型的相鄰關係才連邊"
    (is (= [["a1" "x1"] ["a2" "b1"] ["a3" "b2"] ["iA"]]
           (:batches (tree/writing-order nodes [(rel "x1" "b1" :causal)])))
        "x1→b1 已確認：b1 在 x1 之後；a1 與 x1 並行（a3→x1 沒有關係）")
    (is (= [["a1" "x1" "b1"] ["a2" "b2"] ["a3"] ["iA"]]
           (:batches (tree/writing-order nodes [{:relation/from "x1" :relation/to "b1" :relation/type :causal :relation/pending true}
                                                {:relation/from "a3" :relation/to "x1" :relation/pending false}])))
        "待確認或沒有類型的不連邊")
    (is (= [["a1"] ["a2"] ["a3"] ["x1"] ["iA" "b1"] ["b2"]]
           (:batches (tree/writing-order nodes [(rel "a3" "x1" :causal) (rel "x1" "b1" :causal)])))
        "連邊後 x1 在 a3 之後、b1 在 x1 之後"))
  (testing "依賴：因果、遞進跨標題照樣成立"
    (is (some #{{:id "x1" :dependency :argument :reason "因果承接"}}
              (tree/dependents nodes [(rel "a3" "x1" :causal)] "a3")))
    (is (some #{{:id "b1" :dependency :argument :reason "遞進承接"}}
              (tree/dependents nodes [(rel "x1" "b1" :progression)] "x1")))))

;; ---------------------------------------------------------------------------
;; 附錄（SPEC 5.3）

;; 第一章 甲、第二章 乙、附錄 A 丙（A.1 丙一 → A.1.1 丙一一 → A.1.1.1）、附錄 B 丁
(def app-nodes
  (-> (tree/from-nested
       [(h "A" "甲" (p "a1"))
        (h "B" "乙" (p "b1"))
        (h "X" "丙" (h "X1" "丙一" (h "X11" "丙一一" (h "X111" "丙一一一" (p "x1")))))
        (h "Y" "丁" (p "y1"))])
      (assoc-in ["X" :node/appendix] true)
      (assoc-in ["Y" :node/appendix] true)))

(deftest appendix-numbering-test
  (let [nums (tree/numbering app-nodes)]
    (testing "一般章不受附錄影響"
      (is (= "1" (nums "A")))
      (is (= "2" (nums "B"))))
    (testing "附錄為字母，下層為 A.1、A.1.1、A.1.1.1"
      (is (= {"X" "A" "X1" "A.1" "X11" "A.1.1" "X111" "A.1.1.1" "Y" "B"}
             (select-keys nums ["X" "X1" "X11" "X111" "Y"]))))
    (testing "顯示名稱與位置"
      (is (= "第二章 乙" (tree/display-title app-nodes nums "B")))
      (is (= "附錄 A 丙" (tree/display-title app-nodes nums "X")))
      (is (= "A.1 丙一" (tree/display-title app-nodes nums "X1")))
      (is (= "附錄 B 丁" (tree/display-title app-nodes nums "Y")))
      (is (= "附錄 B" (tree/position-label app-nodes nums "y1")))
      (is (= "A.1.1.1" (tree/position-label app-nodes nums "x1")))
      (is (= "A.1.1.1" (tree/heading-number app-nodes nums "X111")))
      (is (nil? (tree/heading-number app-nodes nums "a1")))))
  (testing "沒有附錄時不變"
    (is (= {"A" "1" "A1" "1.1" "A2" "1.2" "A21" "1.2.1" "A211" "1.2.1.1" "B" "2"} (tree/numbering nodes))))
  (testing "第 27 個以後不丟例外"
    (is (= ["A" "B" "Z" "AA" "AB" "AZ" "BA"] (map tree/appendix-letter [0 1 25 26 27 51 52])))
    (let [many (-> (tree/from-nested (mapv #(h (str "c" %) "附") (range 30)))
                   (as-> ns (reduce #(assoc-in %1 [%2 :node/appendix] true) ns (keys ns))))
          nums (tree/numbering many)]
      (is (= 30 (count (set (vals nums))))))))

(deftest appendix-predicate-test
  (is (tree/appendix? app-nodes "X"))
  (is (not (tree/appendix? app-nodes "A")))
  (is (not (tree/appendix? app-nodes "X1")) "只有章可以是附錄")
  (is (not (tree/appendix? (assoc-in app-nodes ["X1" :node/appendix] true) "X1"))))

(deftest appendix-order-test
  (testing "一般章不能落在附錄之後，附錄不能落在一般章之前"
    (is (nil? (tree/placement app-nodes "A" "X" :after)))
    (is (nil? (tree/placement app-nodes "A" "Y" :before)) "附錄前面的位置在附錄區")
    (is (nil? (tree/placement app-nodes "A" "Y" :after)))
    (is (nil? (tree/placement app-nodes "X" "A" :before)))
    (is (nil? (tree/placement app-nodes "Y" "A" :before)))
    (is (nil? (tree/sibling-placement app-nodes "B" 1)) "一般章下移不能越過附錄")
    (is (nil? (tree/sibling-placement app-nodes "X" -1)) "附錄上移不能越過一般章"))
  (testing "同一區內可以調整順序"
    (is (= {:parent nil :index 0} (tree/placement app-nodes "B" "A" :before)))
    (is (= {:parent nil :index 2} (tree/placement app-nodes "Y" "X" :before))
        "附錄 B 放到附錄 A 之前：在移動者之外的兄弟中計算，前面有兩個一般章")
    (is (= {:parent nil :index 3} (tree/placement app-nodes "X" "Y" :after)))
    (is (= {:parent nil :index 1} (tree/placement app-nodes "A" "B" :after)) "一般章排在最後一個一般章之後（附錄之前）"))
  (testing "附錄章移到標題底下時不再是附錄"
    (let [place (tree/placement app-nodes "Y" "A" :into)
          moved (tree/move app-nodes "Y" place)]
      (is (some? place))
      (is (not (contains? (get moved "Y") :node/appendix))))))

(deftest appendix-insert-key-test
  (testing "新增一般章放在最後一個一般章之後（附錄之前）"
    (let [k (tree/insert-key app-nodes nil :heading)
          ns (assoc app-nodes "N" {:node/id "N" :node/kind :heading :node/name "新" :node/order-key k})]
      (is (= ["A" "B" "N" "X" "Y"] (tree/child-ids ns nil)))))
  (testing "新增附錄章放在最後"
    (let [k (tree/insert-key app-nodes nil :heading true)
          ns (assoc app-nodes "N" {:node/id "N" :node/kind :heading :node/name "新" :node/appendix true :node/order-key k})]
      (is (= ["A" "B" "X" "Y" "N"] (tree/child-ids ns nil)))))
  (testing "沒有附錄時一般章放最後"
    (let [k (tree/insert-key nodes nil :heading)
          ns (assoc nodes "N" {:node/id "N" :node/kind :heading :node/name "新" :node/order-key k})]
      (is (= ["A" "B" "N"] (tree/child-ids ns nil))))))

(deftest set-appendix-test
  (testing "設為附錄：移到附錄區最後"
    (let [ns (tree/set-appendix app-nodes "A" true)]
      (is (= ["B" "X" "Y" "A"] (tree/child-ids ns nil)))
      (is (tree/appendix? ns "A"))
      (is (= "附錄 C" (tree/heading-number ns (tree/numbering ns) "A")))
      (is (= "1" ((tree/numbering ns) "B")))))
  (testing "取消附錄：移到一般章最後"
    (let [ns (tree/set-appendix app-nodes "Y" false)]
      (is (= ["A" "B" "Y" "X"] (tree/child-ids ns nil)))
      (is (not (tree/appendix? ns "Y")))
      (is (not (contains? (get ns "Y") :node/appendix)))
      (is (= "3" ((tree/numbering ns) "Y")))))
  (testing "已是目標狀態、不是章時不變"
    (is (= app-nodes (tree/set-appendix app-nodes "X" true)))
    (is (= app-nodes (tree/set-appendix app-nodes "A" false)))
    (is (= app-nodes (tree/set-appendix app-nodes "X1" true)))
    (is (= app-nodes (tree/set-appendix app-nodes "a1" true)))))

(deftest appendix-char-counts-test
  (let [doc (fn [s] {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text s}]}]})
        ns (-> app-nodes
               (assoc-in ["a1" :node/prose] (doc "一二三"))
               (assoc-in ["b1" :node/prose] (doc "四五"))
               (assoc-in ["x1" :node/prose] (doc "六七八九"))
               (assoc-in ["y1" :node/prose] (doc "十")))
        state {:facts {} :nodes ns}
        counts (tree/char-counts state)]
    (testing "附錄自己的合計照常"
      (is (= 4 (counts "X")))
      (is (= 1 (counts "Y"))))
    (testing "報告本文的合計不含附錄"
      (is (= 5 (tree/report-chars state))))
    (testing "沒有附錄時等於全部"
      (is (= 5 (tree/report-chars {:facts {} :nodes (dissoc ns "X" "X1" "X11" "X111" "x1" "Y" "y1")}))))))
