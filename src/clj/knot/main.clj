(ns knot.main
  (:require [clojure.java.process :as process]
            [integrant.core :as ig]
            [knot.system :as system]))

(defn- open-browser! [url]
  (let [os (System/getProperty "os.name")
        cmd (cond
              (re-find #"(?i)mac" os) ["open" url]
              (re-find #"(?i)win" os) ["cmd" "/c" "start" url]
              :else ["xdg-open" url])]
    (try
      (apply process/start cmd)
      (catch Exception _
        (println "請以瀏覽器開啟" url)))))

(defn -main [& _]
  (let [cfg (system/config)
        sys (ig/init cfg)]
    (.addShutdownHook (Runtime/getRuntime) (Thread. #(ig/halt! sys)))
    (open-browser! (str "http://localhost:" (get-in cfg [:knot/config :http/port])))))
