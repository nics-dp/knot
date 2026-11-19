(ns knot.system
  (:require [integrant.core :as ig]
            [knot.events]
            [knot.server]))

(def defaults
  ;; 報告目錄預設為啟動時所在的目錄
  {:http/port 3000
   ;; 只綁本機；容器內由 KNOT_HOST 改成 0.0.0.0（對外仍由 docker -p 127.0.0.1: 限制）
   :http/host "127.0.0.1"
   :report-dir (System/getProperty "user.dir")
   ;; 開發模式（dev/user.clj 設為 true）：畫面與 API 提供「匯入範例內容」
   :dev? false})

(defmethod ig/init-key :knot/config [_ config] config)

(defn config
  ([] (config {}))
  ([overrides]
   {:knot/config (merge defaults overrides)
    :knot/db {:config (ig/ref :knot/config)}
    :knot/events {:db (ig/ref :knot/db)}
    :knot/http {:config (ig/ref :knot/config)
                :db (ig/ref :knot/db)
                :events (ig/ref :knot/events)}}))
