(ns knot.server-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [knot.db :as db]
            [knot.seed :as seed]
            [knot.server :as server]
            [knot.sync :as sync]
            [knot.ui :as ui]))

(def ^:dynamic *handler* nil)
(def ^:dynamic *store* nil)

(use-fixtures :each
  (fn [t]
    (let [dir (str (java.nio.file.Files/createTempDirectory "knot-api" (make-array java.nio.file.attribute.FileAttribute 0)))
          store (db/open-store dir)]
      (try
        (binding [*store* store *handler* (server/handler store)] (t))
        (finally (db/close-store store))))))

(defn- request [method uri & [body]]
  (let [res (*handler* (cond-> {:request-method method
                                :uri uri
                                :headers {"accept" "application/edn"}}
                         body (-> (assoc :body (java.io.ByteArrayInputStream. (.getBytes (pr-str body) "UTF-8")))
                                  (assoc-in [:headers "content-type"] "application/edn"))))]
    (assoc res :body (some-> (:body res) slurp not-empty edn/read-string))))

(deftest health-test
  (is (= {:status "ok"} (:body (request :get "/api/health")))))

(deftest index-test
  (let [res (*handler* {:request-method :get :uri "/"})]
    (is (= 200 (:status res)))
    (is (= "no-cache" (get-in res [:headers "Cache-Control"])))))

(deftest report-api-test
  (let [id (seed/seed! *store* (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))
        path (str "/api/reports/" id)
        loaded (:body (request :get path))
        state {:nodes (into {} (map (juxt :node/id identity)) (:nodes loaded))
               :relations (:relations loaded)
               :ui {} :history []}
        para (some #(when (= "主要成果" (:node/title %)) (:node/id %)) (:nodes loaded))]
    (is (= [id] (map :report/id (:body (request :get "/api/reports")))))
    (is (= 47 (count (:nodes loaded))))
    (testing "送出修改組"
      (let [changes (sync/diff state (ui/set-node-field state para :node/draft "改過的意圖"))
            res (request :post (str path "/changes") {:label "編輯意圖" :changes changes})]
        (is (= 200 (:status res)))
        (is (uuid? (get-in res [:body :changeset/id])))
        (is (= "改過的意圖" (some #(when (= para (:node/id %)) (:node/draft %))
                             (:nodes (:body (request :get path))))))))
    (testing "格式不正確的修改組被拒絕"
      (is (= 400 (:status (request :post (str path "/changes") {:changes {:nodes {:upsert [{:node/id "x"}]}}})))))
    (testing "復原"
      (let [res (request :post (str path "/undo"))]
        (is (= "編輯意圖" (get-in res [:body :label])))
        (is (not= "改過的意圖" (some #(when (= para (:node/id %)) (:node/draft %))
                                (get-in res [:body :state :nodes]))))))
    (is (= 404 (:status (request :get (str "/api/reports/" (random-uuid))))))))
