(ns knot.api
  "報告資料的 HTTP API。請求與回應都是 EDN（muuntaja）；寫入一律經 knot.db/apply-changes!。"
  (:require [datalevin.core :as d]
            [knot.db :as db]
            [knot.seed :as seed]
            [malli.core :as m]
            [malli.error :as me]))

(def ChangeSet
  "前端送來的一個修改組（knot.sync/diff 的格式）。"
  (let [coll (fn [entity] [:map {:closed true}
                           [:upsert {:optional true} [:vector entity]]
                           [:delete {:optional true} [:vector any?]]])]
    [:map
     [:origin {:optional true} [:enum :manual :move :propagation :regenerate]]
     [:label {:optional true} string?]
     [:changes [:map {:closed true}
                [:nodes {:optional true}
                 (coll [:map
                        [:node/id uuid?]
                        [:node/kind [:enum :heading :intro :paragraph :table :figure]]
                        [:node/parent [:maybe uuid?]]
                        [:node/order-key string?]])]
                [:relations {:optional true}
                 (coll [:map [:relation/from uuid?] [:relation/to uuid?]])]
                [:blocks {:optional true}
                 (coll [:map [:block/id uuid?] [:block/kind [:enum :para :table]]])]
                [:facts {:optional true}
                 (coll [:map [:fact/key string?]])]]]]))

(defn- bad-request [msg & [data]]
  {:status 400 :body (cond-> {:error msg} data (assoc :details data))})

(defn- with-report
  "取出路徑中的報告；不存在時回 404。"
  [store req f]
  (let [id (parse-uuid (get-in req [:path-params :id]))
        c (when id (db/conn store id))]
    (if c
      (f id c)
      {:status 404 :body {:error "找不到報告"}})))

(defn routes [store]
  [["/reports"
    {:get (fn [_] {:status 200 :body (db/list-reports store)})}]
   ["/dev/sample-report"
    ;; 開發用：以範例報告建立一份新報告（本機測試報告優先）
    {:post (fn [_]
             (if-let [fx (seed/sample-fixture)]
               {:status 201 :body {:report/id (seed/seed! store fx)}}
               {:status 404 :body {:error "找不到範例報告"}}))}]
   ["/reports/:id"
    {:get (fn [req]
            (with-report store req
              (fn [_ c] {:status 200 :body (db/load-state (d/db c))})))}]
   ["/reports/:id/changes"
    {:post (fn [req]
             (with-report store req
               (fn [_ c]
                 (let [body (:body-params req)]
                   (if-not (m/validate ChangeSet body)
                     (bad-request "修改內容格式不正確" (me/humanize (m/explain ChangeSet body)))
                     {:status 200 :body {:changeset/id (db/apply-changes! c body)}})))))}]
   ["/reports/:id/undo"
    {:post (fn [req]
             (with-report store req
               (fn [_ c]
                 (let [label (db/undo! c)]
                   {:status 200 :body {:label label :state (db/load-state (d/db c))}}))))}]])
