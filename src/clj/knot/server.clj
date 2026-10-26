(ns knot.server
  (:require [integrant.core :as ig]
            [muuntaja.core :as m]
            [org.httpkit.server :as http]
            [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as muuntaja]))

(defn routes []
  [["/api"
    ["/health" {:get (fn [_] {:status 200 :body {:status "ok"}})}]]])

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
