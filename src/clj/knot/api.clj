(ns knot.api
  "報告資料的 HTTP API。請求與回應都是 EDN（muuntaja）；寫入一律經 knot.db/apply-changes!。"
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [knot.db :as db]
            [knot.notes :as notes]
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

(def NewNote
  [:map {:closed true}
   [:node [:maybe uuid?]]
   [:text string?]
   [:todo {:optional true} boolean?]
   [:reply-to {:optional true} [:maybe uuid?]]])

(def NoteUpdate
  [:or
   [:map {:closed true} [:todo boolean?]]
   [:map {:closed true} [:state [:enum :open :done]]]])

(defn- bad-request [msg & [data]]
  {:status 400 :body (cond-> {:error msg} data (assoc :details data))})

(defn- note-result
  "留言操作的回應：成功時回傳所有留言；錯誤依 error-status 對應狀態碼。"
  [conn res error-status]
  (if-let [msg (:error res)]
    {:status (error-status msg) :body {:error msg}}
    {:status 200 :body {:notes (notes/list-notes (d/db conn))}}))

(def ^:private default-history-limit 10)

(defn- history-limit
  "查詢字串的 limit：沒有或不合法時為預設值，\"all\" 為 nil（全部）。"
  [req]
  (let [v (some->> (:query-string req) (re-find #"(?:^|&)limit=([^&]*)") second)]
    (cond
      (= "all" v) nil
      (and v (re-matches #"\d+" v) (pos? (parse-long v))) (parse-long v)
      :else default-history-limit)))

(defn- history-body
  "最近 limit 筆（舊到新）與是否還有更早的；limit 為 nil 時全部。"
  [entries limit]
  (let [n (count entries)]
    (if (and limit (> n limit))
      {:entries (subvec entries (- n limit)) :more? true}
      {:entries entries :more? false})))

(defn routes [conn]
  [["/notes"
    {:post (fn [req]
             (let [body (:body-params req)]
               (if-not (m/validate NewNote body)
                 (bad-request "留言格式不正確" (me/humanize (m/explain NewNote body)))
                 (note-result conn
                              (notes/add-note! conn (assoc body :author :user))
                              (fn [msg] (if (str/starts-with? msg "找不到") 404 400))))))}]
   ["/notes/:id"
    {:post (fn [req]
             (let [body (:body-params req)
                   id (parse-uuid (get-in req [:path-params :id]))]
               (cond
                 (nil? id) (bad-request "留言 id 格式不正確")
                 (not (m/validate NoteUpdate body)) (bad-request "留言格式不正確" (me/humanize (m/explain NoteUpdate body)))
                 :else (note-result conn
                                    (if (contains? body :todo)
                                      (notes/set-todo! conn id (:todo body))
                                      (notes/set-state! conn id (:state body) {}))
                                    (fn [msg] (if (= "找不到留言" msg) 404 400))))))
     :delete (fn [req]
               (if-let [id (parse-uuid (get-in req [:path-params :id]))]
                 (note-result conn (notes/delete-note! conn id :user)
                              (fn [msg] (case msg "找不到留言" 404 "只能刪除自己的留言" 403 400)))
                 (bad-request "留言 id 格式不正確")))}]
   ["/report"
    {:get (fn [_] {:status 200 :body (db/load-state (d/db conn))})}]
   ["/report/changes"
    {:post (fn [req]
             (let [body (:body-params req)]
               (if-not (m/validate ChangeSet body)
                 (bad-request "修改內容格式不正確" (me/humanize (m/explain ChangeSet body)))
                 (let [res (db/apply-changes! conn (assoc body :by :user :check-versions? true))]
                   (if-let [conflicts (:conflicts res)]
                     {:status 409 :body {:error "卡片已被其他來源修改" :conflicts conflicts}}
                     {:status 200 :body (or res {:versions {}})})))))}]
   ["/report/nodes/:id/previous"
    {:get (fn [req]
            (if-let [id (parse-uuid (get-in req [:path-params :id]))]
              {:status 200 :body (db/node-previous (d/db conn) id)}
              (bad-request "卡片 id 格式不正確")))}]
   ["/report/nodes/:id/history"
    {:get (fn [req]
            (if-let [id (parse-uuid (get-in req [:path-params :id]))]
              {:status 200 :body (history-body (db/node-history (d/db conn) id) (history-limit req))}
              (bad-request "卡片 id 格式不正確")))}]
   ["/report/nodes/:id/changes/:changeset-id"
    {:get (fn [req]
            (let [{:keys [id changeset-id]} (:path-params req)
                  id (parse-uuid id) cs (parse-uuid changeset-id)]
              (if-not (and id cs)
                (bad-request "id 格式不正確")
                (if-let [change (db/node-change (d/db conn) id cs)]
                  {:status 200 :body change}
                  {:status 404 :body {:error "這張卡片沒有這次修改"}}))))}]
   ["/report/recent-changes"
    {:get (fn [_] {:status 200 :body {:cards (db/recent-cards (d/db conn) 10)}})}]
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
