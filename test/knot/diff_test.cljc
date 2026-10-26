(ns knot.diff-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.diff :as diff]))

(defn- texts [ops op]
  (keep (fn [[o s]] (when (= o op) s)) ops))

(deftest diff-test
  (testing "中文逐字比較，相鄰同類合併"
    (let [ops (diff/diff "本案驗證範圍沿用模式。" "本案驗證範圍改採模式，另增一項。")]
      (is (= ["沿用"] (texts ops :del)))
      (is (= ["改採" "，另增一項"] (texts ops :ins)))
      (is (= "本案驗證範圍改採模式，另增一項。" (apply str (map second (remove #(= :del (first %)) ops)))))
      (is (= "本案驗證範圍沿用模式。" (apply str (map second (remove #(= :ins (first %)) ops)))))))
  (testing "英數字以整串為單位"
    (is (= [[:same "版本 "] [:del "v0.3"] [:ins "v0.4"]] (diff/diff "版本 v0.3" "版本 v0.4"))))
  (testing "空字串與 nil"
    (is (= [[:ins "新的"]] (diff/diff nil "新的")))
    (is (= [[:del "舊的"]] (diff/diff "舊的" ""))))
  (testing "changed?"
    (is (not (diff/changed? (diff/diff "相同" "相同"))))
    (is (diff/changed? (diff/diff "甲" "乙")))))
