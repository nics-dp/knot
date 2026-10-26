(ns knot.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.tree :as tree]
            [knot.ui :as ui]))

(def report
  {:report/title "測試報告"
   :ui/selected "a1"
   :tree [{:node/id "A" :node/kind :heading :node/name "甲"
           :children [{:node/id "a1" :node/kind :paragraph :node/title "一" :node/status :ai}
                      {:node/id "a2" :node/kind :paragraph :node/title "二" :node/status :human}]}
          {:node/id "B" :node/kind :heading :node/name "乙"
           :children [{:node/id "b1" :node/kind :paragraph :node/title "三" :node/status :none}]}]
   :relations [{:relation/from "a1" :relation/to "a2" :relation/type :causal}]})

(def state (ui/init-state report))

(deftest navigate-test
  (is (= "a2" (get-in (ui/navigate state :down) [:ui :selected])))
  (is (= "A" (get-in (ui/navigate state :left) [:ui :selected])))
  (is (= "a1" (get-in (ui/navigate state :up) [:ui :selected])) "沒有上一個兄弟時不動"))

(deftest drag-and-drop-test
  (let [s (-> state
              (ui/drag-start "a1")
              (ui/drag-over "B" :top))]
    (is (= {:id "B" :pos :into} (get-in s [:ui :over])))
    (let [s (ui/drop-dragged s)]
      (testing "移動後被移動的卡片成為選取狀態，並顯示訊息"
        (is (= "a1" (get-in s [:ui :selected])))
        (is (= ["b1" "a1"] (tree/child-ids (:nodes s) "B")))
        (is (re-find #"待確認" (get-in s [:ui :toast]))))
      (testing "拖曳狀態清除"
        (is (nil? (get-in s [:ui :drag]))))
      (testing "整個移動可以復原"
        (let [u (ui/undo s)]
          (is (= (:nodes state) (:nodes u)))
          (is (= (:relations state) (:relations u)))
          (is (empty? (:history u))))))))

(deftest illegal-drop-test
  (let [s (-> state (ui/drag-start "a1") (ui/drag-over "a1" :top))]
    (is (nil? (get-in s [:ui :over])))
    (is (= (:nodes state) (:nodes (ui/drop-dragged s))))))

(deftest move-selected-test
  (let [s (ui/move-selected (ui/select state "a2") -1)]
    (is (= ["a2" "a1"] (tree/child-ids (:nodes s) "A")))
    (is (:relation/pending (tree/relation-between (:relations s) "a2" "a1")))))

(deftest add-child-test
  (testing "新增引言放在最前面"
    (let [s (ui/add-child state "A" :intro)
          id (get-in s [:ui :selected])]
      (is (= :intro (tree/kind (:nodes s) id)))
      (is (= id (first (tree/child-ids (:nodes s) "A"))))
      (testing "引言最多一張"
        (is (= (:nodes s) (:nodes (ui/add-child s "A" :intro)))))))
  (testing "已有段落的標題不能直接新增下層標題"
    (is (= (:nodes state) (:nodes (ui/add-child state "A" :heading)))))
  (testing "把現有段落移到新的下層標題"
    (let [s (ui/wrap-paragraphs state "A")
          sub (get-in s [:ui :selected])]
      (is (= [sub] (tree/child-ids (:nodes s) "A")))
      (is (= ["a1" "a2"] (tree/child-ids (:nodes s) sub))))))

(deftest paragraph->intro-test
  (let [s (ui/paragraph->intro state "a1")]
    (is (= :intro (tree/kind (:nodes s) "a1")))
    (is (= ["a1" "a2"] (tree/child-ids (:nodes s) "A")))
    (is (empty? (:relations s)) "引言不再有段落關係")))

(deftest toggle-lock-test
  (let [s (ui/toggle-lock state "a2")]
    (is (= :locked (get-in s [:nodes "a2" :node/status])))
    (is (= :human (get-in (ui/toggle-lock s "a2") [:nodes "a2" :node/status])))))
