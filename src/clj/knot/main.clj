(ns knot.main
  (:require [clojure.java.process :as process]
            [clojure.string :as str]
            [integrant.core :as ig]
            [knot.system :as system])
  (:gen-class))

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

(defn env-overrides
  "由環境變數（map，通常是 (System/getenv)）得到 system/config 的覆寫；沒設就不覆寫。
  KNOT_HOST → :http/host"
  [env]
  (let [host (get env "KNOT_HOST")]
    (cond-> {}
      (not (str/blank? host)) (assoc :http/host host))))

(defn no-browser?
  "環境變數 KNOT_NO_BROWSER 有值時不開瀏覽器（容器內使用）。"
  [env]
  (not (str/blank? (get env "KNOT_NO_BROWSER"))))

(defn -main
  "java -jar knot.jar [報告目錄]：讀取目錄中的報告，沒有時建立一份空白報告。"
  [& [report-dir]]
  (let [env (System/getenv)
        cfg (system/config (cond-> (env-overrides env)
                             report-dir (assoc :report-dir report-dir)))
        sys (ig/init cfg)
        url (str "http://localhost:" (get-in cfg [:knot/config :http/port]))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. #(ig/halt! sys)))
    (if (no-browser? env)
      (println "請以瀏覽器開啟" url)
      (open-browser! url))))
