(ns knot.server-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [knot.db :as db]
            [knot.notes :as notes]
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
  (let [[path query] (str/split uri #"\?" 2)
        res (*handler* (cond-> {:request-method method
                                :uri path
                                :query-string query
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
        (is (= {para 2} (get-in res [:body :versions])) "回傳寫入後的版本")
        (is (= "改過的意圖" (some #(when (= para (:node/id %)) (:node/draft %))
                              (:nodes (:body (request :get "/api/report"))))))))
    (testing "卡片的前一版"
      (is (= {} (:body (request :get (str "/api/report/nodes/" para "/previous"))))
          "只改了意圖，本文沒有前一版")
      (is (= 400 (:status (request :get "/api/report/nodes/not-a-uuid/previous")))))
    (testing "卡片歷程"
      (let [body (:body (request :get (str "/api/report/nodes/" para "/history")))]
        (is (= [:import :change] (mapv :kind (:entries body))))
        (is (false? (:more? body)))
        (is (= #{:intent} (:fields (last (:entries body))))))
      (let [body (:body (request :get (str "/api/report/nodes/" para "/history?limit=1")))]
        (is (= 1 (count (:entries body))))
        (is (= :change (:kind (first (:entries body)))) "最近的一筆")
        (is (true? (:more? body))))
      (is (= 2 (count (:entries (:body (request :get (str "/api/report/nodes/" para "/history?limit=all")))))))
      (is (= {:entries [] :more? false} (:body (request :get (str "/api/report/nodes/" (random-uuid) "/history")))))
      (is (= 400 (:status (request :get "/api/report/nodes/not-a-uuid/history")))))
    (testing "某次修改的本文前後：只改意圖時標明沒有改本文，找不到時為 404"
      (let [cid (first (:changeset-ids (last (:entries (:body (request :get (str "/api/report/nodes/" para "/history")))))))]
        (is (false? (:prose? (:body (request :get (str "/api/report/nodes/" para "/changes/" cid))))))
        (is (= 404 (:status (request :get (str "/api/report/nodes/" para "/changes/" (random-uuid))))))
        (is (= 400 (:status (request :get (str "/api/report/nodes/" para "/changes/xyz")))))))
    (testing "版本已過期：409 並列出衝突"
      (let [stale (sync/diff state (ui/set-node-field state para :node/draft "過期的修改"))
            res (request :post "/api/report/changes" {:label "過期" :changes stale})]
        (is (= 409 (:status res)))
        (is (= "卡片已被其他來源修改" (get-in res [:body :error])))
        (is (= [{:node/id para :expected 1 :actual 2}] (get-in res [:body :conflicts])))))
    (testing "格式不正確的修改組被拒絕"
      (is (= 400 (:status (request :post "/api/report/changes" {:changes {:nodes {:upsert [{:node/id "x"}]}}})))))
    (testing "復原"
      (let [res (request :post "/api/report/undo")]
        (is (= "編輯意圖" (get-in res [:body :label])))
        (is (not= "改過的意圖" (some #(when (= para (:node/id %)) (:node/draft %))
                                  (get-in res [:body :state :nodes]))))))
    (testing "已有內容時不能匯入範例"
      (is (= 409 (:status (request :post "/api/dev/sample")))))))

(deftest notes-api-test
  (seed/seed! *conn* (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))
  (let [card (some #(when (= "主要成果" (:node/title %)) (:node/id %)) (:nodes (:body (request :get "/api/report"))))
        post-note (fn [body] (request :post "/api/notes" body))]
    (is (= [] (:notes (:body (request :get "/api/report")))))
    (testing "新增留言：回傳所有留言"
      (let [res (post-note {:node card :text "請補充數據" :todo true})
            note (first (get-in res [:body :notes]))]
        (is (= 200 (:status res)))
        (is (= {:note/node card :note/author :user :note/state :open :note/todo true}
               (select-keys note [:note/node :note/author :note/state :note/todo])))
        (is (= (:notes (:body (request :get "/api/report"))) (get-in res [:body :notes])))
        (testing "完成與重開"
          (let [r (request :post (str "/api/notes/" (:note/id note)) {:state :done})]
            (is (= :done (get-in r [:body :notes 0 :note/state]))))
          (is (= :open (get-in (request :post (str "/api/notes/" (:note/id note)) {:state :open})
                               [:body :notes 0 :note/state]))))
        (testing "取消待辦"
          (is (not (contains? (get-in (request :post (str "/api/notes/" (:note/id note)) {:todo false})
                                      [:body :notes 0])
                              :note/todo))))
        (testing "回覆"
          (let [r (post-note {:node nil :text "好" :reply-to (:note/id note)})]
            (is (= 200 (:status r)))
            (is (= card (get-in r [:body :notes 1 :note/node])))))))
    (testing "錯誤"
      (is (= 400 (:status (post-note {:node card :text "  "}))))
      (is (= 400 (:status (post-note {:text "缺少 node"}))))
      (is (= 404 (:status (post-note {:node (random-uuid) :text "x"})))
          "不存在的卡片")
      (is (= 404 (:status (request :post (str "/api/notes/" (random-uuid)) {:todo true}))))
      (is (= 400 (:status (request :post (str "/api/notes/" (random-uuid)) {:state :weird}))))
      (is (= 404 (:status (request :delete (str "/api/notes/" (random-uuid)))))))
    (testing "刪除：自己的連同回覆；別人的 403"
      (let [theirs (:note/id (notes/add-note! *conn* {:node card :author :agent :text "agent 的問題"}))
            mine (get-in (post-note {:node card :text "我的"}) [:body :notes])
            my-id (:note/id (last (filter #(= :user (:note/author %)) mine)))]
        (is (= 403 (:status (request :delete (str "/api/notes/" theirs)))))
        (let [res (request :delete (str "/api/notes/" my-id))]
          (is (= 200 (:status res)))
          (is (not-any? #(= my-id (:note/id %)) (get-in res [:body :notes])))
          (is (some #(= theirs (:note/id %)) (get-in res [:body :notes]))))))))

(deftest report-info-test
  (let [info #(:report (:body %))]
    (testing "改名稱與副標"
      (let [res (request :post "/api/report/info" {:title "新名稱" :subtitle "副標"})]
        (is (= 200 (:status res)))
        (is (= ["新名稱" "副標"] ((juxt :report/title :report/subtitle) (info res))))
        (is (= "新名稱" (get-in (request :get "/api/report") [:body :report :report/title])))))
    (testing "只給副標；空字串清除"
      (let [res (request :post "/api/report/info" {:subtitle ""})]
        (is (= ["新名稱" nil] ((juxt :report/title :report/subtitle) (info res))))))
    (testing "不屬於修改組"
      (is (nil? (db/undo! *conn*))))
    (testing "格式錯誤"
      (is (= 400 (:status (request :post "/api/report/info" {}))))
      (is (= 400 (:status (request :post "/api/report/info" {:title "  "}))))
      (is (= 400 (:status (request :post "/api/report/info" {:title 1}))))
      (is (= 400 (:status (request :post "/api/report/info" {:title "x" :other 1})))))))

(deftest style-changes-api-test
  (let [res (request :post "/api/report/changes"
                     {:label "編輯文字" :changes {:style {:upsert [{:style/key "report" :style/text "風格"}]}}})]
    (is (= 200 (:status res)))
    (is (= {:style/key "report" :style/text "風格"} (:style (:body (request :get "/api/report")))))
    (is (= {:updated-by :user} (select-keys (:style-info (:body (request :get "/api/report"))) [:updated-by])))
    (is (= 400 (:status (request :post "/api/report/changes"
                                 {:changes {:style {:upsert [{:style/key "other" :style/text "x"}]}}}))))))
