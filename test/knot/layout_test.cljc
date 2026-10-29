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

(deftest mixed-columns-test
  (let [nodes (tree/from-nested
               [{:node/id "A" :node/kind :heading
                 :children [{:node/id "iA" :node/kind :intro}
                            {:node/id "p1" :node/kind :paragraph}
                            {:node/id "p2" :node/kind :paragraph}
                            {:node/id "A1" :node/kind :heading
                             :children [{:node/id "a1" :node/kind :paragraph}]}]}])
        cols (layout/columns nodes)]
    (testing "同一 parent 的段落與下層標題在同一欄，段落在前"
      (is (= [["A"] ["iA" "p1" "p2" "A1"] ["a1"]] (mapv #(mapv :id %) cols))))
    (testing "同一組：段落之間留關係空間，段落與下層標題之間是組內間距"
      (is (= [0 layout/gap-in-group layout/gap-between-paragraphs layout/gap-in-group]
             (mapv :gap-before (second cols)))))
    (testing "選取下層標題：子孫欄對齊它底下的段落"
      (is (= 3 (count (layout/offsets nodes cols "A1" 600)))))
    (testing "段落可以拖到下層標題之前，不能拖到之後"
      (is (nil? (tree/placement nodes "p1" "A1" :after)))
      (is (some? (tree/placement nodes "p1" "A1" :before)))
      (let [{:keys [top height]} (last (second cols))
            y (+ top height 4)]
        (is (not= {:id "A1" :pos :after} (layout/drop-target nodes cols "p1" 1 y))
            "指標在最後一張下層標題下緣：不能放在它後面")))))

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

(deftest drop-target-test
  ;; 欄 1：a1（0–104）、a2（134–238）屬 A1；欄 0：A（0–84）、B（92–176）
  (let [cols (layout/columns nodes)
        nodes2 (tree/from-nested
                [{:node/id "A" :node/kind :heading
                  :children [{:node/id "a1" :node/kind :paragraph}
                             {:node/id "a2" :node/kind :paragraph}]}
                 {:node/id "B" :node/kind :heading
                  :children [{:node/id "b1" :node/kind :paragraph}]}
                 {:node/id "C" :node/kind :heading}])
        cols2 (layout/columns nodes2)]
    (testing "同一欄：取最近的合法間隙"
      (is (= {:id "a1" :pos :before} (layout/drop-target nodes2 cols2 "b1" 1 -3)))
      (is (= {:id "a1" :pos :after} (layout/drop-target nodes2 cols2 "b1" 1 112)))
      (is (= {:id "a2" :pos :before} (layout/drop-target nodes2 cols2 "a1" 1 50))
          "停在自己身上：最近的合法位置就是原位，放下後不變"))
    (testing "組與組之間：靠近上方卡片是接在後面，靠近下方卡片是插在前面"
      (let [a2-bottom (+ 104 30 104)
            b1-top (+ a2-bottom layout/gap-between-groups)]
        (is (= {:id "a2" :pos :after} (layout/drop-target nodes2 cols2 "a1" 1 (+ a2-bottom 2))))
        (is (= {:id "b1" :pos :before} (layout/drop-target nodes2 cols2 "a1" 1 (- b1-top 2))))))
    (testing "左邊一欄：移入指標所在的卡片，空標題也可以"
      (is (= {:id "B" :pos :into} (layout/drop-target nodes2 cols2 "a1" 0 120)))
      (is (= {:id "C" :pos :into} (layout/drop-target nodes2 cols2 "a1" 0 500))))
    (testing "其他欄不接受"
      (is (nil? (layout/drop-target nodes cols "t1" 0 10))))))
