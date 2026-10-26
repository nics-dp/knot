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
(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [t]
    (let [dir (str (java.nio.file.Files/createTempDirectory "knot-api" (make-array java.nio.file.attribute.FileAttribute 0)))
          conn (db/open-report dir)]
      (try
        (binding [*conn* conn *handler* (server/handler conn)] (t))
        (finally (db/close-report conn))))))

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
  (seed/seed! *conn* (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))
  (let [loaded (:body (request :get "/api/report"))
        state {:nodes (into {} (map (juxt :node/id identity)) (:nodes loaded))
               :relations (:relations loaded)
               :ui {} :history []}
        para (some #(when (= "主要成果" (:node/title %)) (:node/id %)) (:nodes loaded))]
    (is (= "OO 市智慧路燈示範計畫" (get-in loaded [:report :report/title])))
    (is (= 47 (count (:nodes loaded))))
    (testing "送出修改組"
      (let [changes (sync/diff state (ui/set-node-field state para :node/draft "改過的意圖"))
            res (request :post "/api/report/changes" {:label "編輯意圖" :changes changes})]
        (is (= 200 (:status res)))
        (is (uuid? (get-in res [:body :changeset/id])))
        (is (= "改過的意圖" (some #(when (= para (:node/id %)) (:node/draft %))
                              (:nodes (:body (request :get "/api/report"))))))))
    (testing "格式不正確的修改組被拒絕"
      (is (= 400 (:status (request :post "/api/report/changes" {:changes {:nodes {:upsert [{:node/id "x"}]}}})))))
    (testing "復原"
      (let [res (request :post "/api/report/undo")]
        (is (= "編輯意圖" (get-in res [:body :label])))
        (is (not= "改過的意圖" (some #(when (= para (:node/id %)) (:node/draft %))
                                  (get-in res [:body :state :nodes]))))))
    (testing "已有內容時不能匯入範例"
      (is (= 409 (:status (request :post "/api/dev/sample")))))))
