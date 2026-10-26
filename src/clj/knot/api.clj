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

(defn routes [conn]
  [["/report"
    {:get (fn [_] {:status 200 :body (db/load-state (d/db conn))})}]
   ["/report/changes"
    {:post (fn [req]
             (let [body (:body-params req)]
               (if-not (m/validate ChangeSet body)
                 (bad-request "修改內容格式不正確" (me/humanize (m/explain ChangeSet body)))
                 {:status 200 :body {:changeset/id (db/apply-changes! conn body)}})))}]
   ["/report/nodes/:id/previous"
    {:get (fn [req]
            (if-let [id (parse-uuid (get-in req [:path-params :id]))]
              {:status 200 :body (db/node-previous (d/db conn) id)}
              (bad-request "卡片 id 格式不正確")))}]
   ["/report/undo"
    {:post (fn [_]
             (let [label (db/undo! conn)]
               {:status 200 :body {:label label :state (db/load-state (d/db conn))}}))}]
   ["/dev/sample"
    ;; 開發用：把範例報告匯入空白報告（本機測試報告優先）
    {:post (fn [_]
             (cond
               (not (db/empty-report? (d/db conn)))
               {:status 409 :body {:error "報告已有內容"}}

               :else
               (if-let [fx (seed/sample-fixture)]
                 (do (seed/seed! conn fx)
                     {:status 200 :body (db/load-state (d/db conn))})
                 {:status 404 :body {:error "找不到範例報告"}})))}]])
