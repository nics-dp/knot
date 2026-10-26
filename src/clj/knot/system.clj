(ns knot.system
  (:require [integrant.core :as ig]
            [knot.server]))

(def defaults
  {:http/port 3000
   :data-dir (str (System/getProperty "user.home") "/Knot")})

(defmethod ig/init-key :knot/config [_ config] config)

(defn config
  ([] (config {}))
  ([overrides]
   {:knot/config (merge defaults overrides)
    :knot/http {:config (ig/ref :knot/config)}}))
