(ns knot.server
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [integrant.core :as ig]
            [muuntaja.core :as m]
            [org.httpkit.server :as http]
            [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as muuntaja]))

(defn- sample-report
  "M1 之前的介面開發用：讀取 classpath 上的虛構範例報告（只在 :dev 加入 test-resources 時存在）。"
  [_]
  (if-let [res (io/resource "fixtures/reports/streetlight.edn")]
    {:status 200 :body (edn/read-string (slurp res))}
    {:status 404 :body {:error "找不到範例報告；請以 :dev alias 啟動"}}))

(defn routes []
  [["/api"
    ["/health" {:get (fn [_] {:status 200 :body {:status "ok"}})}]
    ["/sample-report" {:get sample-report}]]])

(defn handler []
  (ring/ring-handler
   (ring/router (routes)
                {:data {:muuntaja m/instance
                        :middleware [muuntaja/format-middleware]}})
   (ring/routes
    (ring/create-resource-handler {:path "/" :root "public"})
    (ring/create-default-handler))))

(defmethod ig/init-key :knot/http [_ {:keys [config]}]
  (let [port (:http/port config)]
    (println (str "Knot 伺服器：http://localhost:" port))
    (http/run-server (handler) {:port port :legacy-return-value? false})))

(defmethod ig/halt-key! :knot/http [_ server]
  @(http/server-stop! server))
