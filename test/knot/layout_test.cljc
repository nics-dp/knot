(ns knot.layout-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.layout :as layout]
            [knot.tree :as tree]))

(def nodes
  (tree/from-nested
   [{:node/id "A" :node/kind :heading
     :children [{:node/id "iA" :node/kind :intro}
                {:node/id "A1" :node/kind :heading
                 :children [{:node/id "a1" :node/kind :paragraph}
                            {:node/id "a2" :node/kind :paragraph
                             :children [{:node/id "t1" :node/kind :table}]}]}]}
    {:node/id "B" :node/kind :heading
     :children [{:node/id "b1" :node/kind :paragraph}]}]))

(deftest columns-test
  (let [cols (layout/columns nodes)]
    (is (= 4 (count cols)))
    (is (= [["A" "B"] ["iA" "A1" "b1"] ["a1" "a2"] ["t1"]]
           (mapv #(mapv :id %) cols)))
    (testing "同一 parent 為一組，組間間距較大；相鄰段落留出關係的空間"
      (let [[c0 c1 c2] cols]
        (is (= [0 layout/gap-in-group] (mapv :gap-before c0)))
        (is (= [0 layout/gap-in-group layout/gap-between-groups] (mapv :gap-before c1)))
        (is (= [0 layout/gap-between-paragraphs] (mapv :gap-before c2)))))
    (testing "位置依序累加"
      (let [[_ c1] cols]
        (is (= (+ (layout/card-height :intro) layout/gap-in-group) (:top (second c1))))))))

(deftest offsets-test
  (let [cols (layout/columns nodes)
        v 600
        center (fn [col id offset]
                 (let [{:keys [top height]} (some #(when (= id (:id %)) %) col)]
                   (+ offset top (/ height 2))))]
    (testing "選取的卡片與祖先對齊欄位中線"
      (let [offs (layout/offsets nodes cols "a2" v)]
        (is (= (/ v 2) (center (nth cols 2) "a2" (nth offs 2))))
        (is (= (/ v 2) (center (nth cols 1) "A1" (nth offs 1))))
        (is (= (/ v 2) (center (nth cols 0) "A" (nth offs 0))))
        (is (= (/ v 2) (center (nth cols 3) "t1" (nth offs 3))) "子孫欄對齊子孫")))
    (testing "子孫欄對齊所有子孫的整體範圍"
      (let [offs (layout/offsets nodes cols "A1" v)
            [a1 a2] (nth cols 2)]
        (is (= (/ v 2) (+ (nth offs 2) (/ (+ (:top a1) (:top a2) (:height a2)) 2))))))
    (testing "範圍高於可視區時改為頂端對齊"
      (let [offs (layout/offsets nodes cols "A" 120)]
        (is (= layout/top-margin (+ (nth offs 1) (:top (first (nth cols 1))))))))))
