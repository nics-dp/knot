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

(deftest chinese-numeral-test
  (is (= ["一" "九" "十" "十一" "二十" "二十三"]
         (map tree/chinese-numeral [1 9 10 11 20 23]))))

(deftest accepts-test
  (testing "最上層只能放標題"
    (is (tree/accepts? nodes nil "A1"))
    (is (not (tree/accepts? nodes nil "a1"))))
  (testing "段落與下層標題不混用"
    (is (tree/accepts? nodes "B" "a1"))
    (is (not (tree/accepts? nodes "A" "a1")))
    (is (not (tree/accepts? nodes "B" "A21"))))
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
