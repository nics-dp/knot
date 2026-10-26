(ns knot.server-test
  (:require [clojure.test :refer [deftest is]]
            [knot.server :as server]))

(deftest health-test
  (let [res ((server/handler) {:request-method :get
                               :uri "/api/health"
                               :headers {"accept" "application/json"}})]
    (is (= 200 (:status res)))
    (is (= "{\"status\":\"ok\"}" (slurp (:body res))))))

(deftest index-test
  (let [res ((server/handler) {:request-method :get :uri "/"})]
    (is (= 200 (:status res)))))
