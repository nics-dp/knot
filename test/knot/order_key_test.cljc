(ns knot.order-key-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.order-key :as ok]))

(defn- ordered? [ks]
  (every? neg? (map compare ks (rest ks))))

(deftest between-test
  (testing "沒有上下界"
    (is (string? (ok/between nil nil))))
  (testing "結果嚴格介於上下界之間"
    (doseq [[a b] [["V" "k"] ["V" "W"] ["V" "V1"] ["0V" "1"] ["z" nil] [nil "1"] ["a" "a0V"]]]
      (let [k (ok/between a b)]
        (is (or (nil? a) (neg? (compare a k))) (str a " < " k))
        (is (or (nil? b) (neg? (compare k b))) (str k " < " b)))))
  (testing "下界不小於上界時拋出例外"
    (is (thrown? #?(:clj Exception :cljs js/Error) (ok/between "b" "a")))))

(deftest repeated-insertion-test
  (testing "一直插在最前面"
    (let [ks (reduce (fn [ks _] (cons (ok/between nil (first ks)) ks)) (list (ok/between nil nil)) (range 50))]
      (is (ordered? ks))
      (is (apply distinct? ks))))
  (testing "一直插在同一個縫隙"
    (let [ks (loop [lo "V" hi "W" acc [] n 0]
               (if (= n 50) acc
                   (let [k (ok/between lo hi)] (recur lo k (conj acc k) (inc n)))))]
      (is (ordered? (reverse ks)))
      (is (every? #(and (neg? (compare "V" %)) (neg? (compare % "W"))) ks))))
  (testing "initial-keys 遞增"
    (let [ks (ok/initial-keys 100)]
      (is (= 100 (count ks)))
      (is (ordered? ks)))))
