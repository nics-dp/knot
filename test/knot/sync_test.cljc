(ns knot.sync-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.sync :as sync]
            [knot.ui :as ui]))

(def state
  (ui/init-state
   {:tree [{:node/id "A" :node/kind :heading :node/name "甲"
            :children [{:node/id "a1" :node/kind :paragraph :node/title "一"}
                       {:node/id "a2" :node/kind :paragraph :node/title "二"}]}]
    :relations [{:relation/from "a1" :relation/to "a2" :relation/type :causal}]
    :blocks [{:block/id "b1" :block/kind :para :block/status :candidate}]
    :facts [{:fact/key "k" :fact/value "1"}]}))

(deftest diff-test
  (is (nil? (sync/diff state state)) "沒有變動")
  (testing "只帶出變動的卡片"
    (let [s (ui/set-node-field state "a1" :node/draft "新意圖")]
      (is (= {:nodes {:upsert [(get-in s [:nodes "a1"])]}} (sync/diff state s)))))
  (testing "移動段落：卡片與關係一起變動"
    (let [d (sync/diff state (ui/move-selected (ui/select state "a2") -1))]
      (is (= ["a2"] (map :node/id (get-in d [:nodes :upsert]))))
      (is (= ["a1|a2|adjacent"] (get-in d [:relations :delete])))
      (is (= [{:relation/from "a2" :relation/to "a1" :relation/type nil :relation/pending true}]
             (get-in d [:relations :upsert])))))
  (testing "刪除卡片"
    (let [d (sync/diff state (ui/delete-node state "a2"))]
      (is (= ["a2"] (get-in d [:nodes :delete])))))
  (testing "素材狀態"
    (is (= {:blocks {:upsert [{:block/id "b1" :block/kind :para :block/status :confirmed}]}}
           (sync/diff state (ui/set-block-status state "b1" :confirmed))))))
