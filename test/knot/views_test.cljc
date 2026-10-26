(ns knot.views-test
  (:require [clojure.test :refer [deftest is]]
            [knot.views :as views]))

(deftest app-test
  (is (= :main.app (first (views/app {})))))
