(ns knot.server
  (:require [integrant.core :as ig]
            [knot.api :as api]
            [knot.db :as db]
            [knot.events :as events]
            [knot.mcp :as mcp]
            [muuntaja.core :as m]
            [org.httpkit.server :as http]
            [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as muuntaja]))

(defn routes [conn events opts]
  [["/api"
    ["/health" {:get (fn [_] {:status 200 :body {:status "ok"}})}]
    ["/events" {:get (if events
                       (events/handler events)
                       (fn [_] {:status 503 :body {:error "事件通知未啟用"}}))}]
    (api/routes conn opts)]])

(defn- revalidate
  "靜態檔每次都向伺服器確認是否更新（未變更時回 304），避免瀏覽器沿用舊的 CSS／JS。"
  [h]
  (fn [req]
    (some-> (h req) (assoc-in [:headers "Cache-Control"] "no-cache"))))

(defn handler
  "events 為 :knot/events 的值；省略時 /api/events 回 503（測試用）。
  opts：{:dev? true} 時提供開發用的 API（匯入範例內容）。"
  ([conn] (handler conn nil {}))
  ([conn events] (handler conn events {}))
  ([conn events opts]
   (let [mcp-handler (mcp/handler conn)
         app (ring/ring-handler
              (ring/router (routes conn events opts)
                           {:data {:muuntaja m/instance
                                   :middleware [muuntaja/format-middleware]}})
              (ring/routes
               (revalidate (ring/create-resource-handler {:path "/" :root "public"}))
               (ring/create-default-handler)))]
     ;; /mcp 收發 JSON，不經過 muuntaja 的 EDN 格式化，所以在路由之前處理
     (fn [req]
       (if (= "/mcp" (:uri req))
         (mcp-handler req)
         (app req))))))

(defmethod ig/init-key :knot/db [_ {:keys [config]}]
  (db/open-report (:report-dir config)))

(defmethod ig/halt-key! :knot/db [_ conn]
  (db/close-report conn))

(defmethod ig/init-key :knot/http [_ {:keys [config db events]}]
  (let [port (:http/port config)]
    (println (str "Knot 伺服器：http://localhost:" port "（報告目錄 "
                  (.getCanonicalPath (java.io.File. (str (:report-dir config)))) "）"))
    (http/run-server (handler db events (select-keys config [:dev?])) {:ip (:http/host config) :port port :legacy-return-value? false})))

(defmethod ig/halt-key! :knot/http [_ server]
  ;; 已停止的伺服器 server-stop! 回傳 nil
  (some-> (http/server-stop! server) deref))
