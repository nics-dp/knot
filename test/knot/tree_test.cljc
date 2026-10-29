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
