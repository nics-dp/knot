(ns knot.server
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [integrant.core :as ig]
            [knot.ingest :as ingest]
            [muuntaja.core :as m]
            [org.httpkit.server :as http]
            [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as muuntaja]))

(def local-sources-dir
  "使用者在本機放的參考資料轉換結果（anydoc 輸出的 Markdown），不進 git。"
  "local/sources")

(defn- local-blocks
  "把 local/sources/*.md 切成候選區塊，模擬素材匯入後的狀態。"
  []
  (let [files (->> (.listFiles (io/file local-sources-dir))
                   (filter #(str/ends-with? (.getName ^java.io.File %) ".md"))
                   (sort-by #(.getName ^java.io.File %)))]
    (vec (mapcat (fn [i ^java.io.File f]
                   (ingest/markdown->blocks
                    (slurp f)
                    {:source (str/replace (.getName f) #"\.md$" ".pdf")
                     :id-prefix (str "ref" (inc i))}))
                 (range) files))))

(def local-sample-report
  "使用者在本機準備的測試報告（節錄自真實參考資料），不進 git。存在時優先使用。"
  "local/sample-report.edn")

(defn- sample-report
  "M1 之前的介面開發用。優先讀取本機測試報告；沒有時讀取 classpath 上的虛構範例報告
  （只在 :dev 加入 test-resources 時存在），再加上本機參考資料切出的區塊。"
  [_]
  (let [local (io/file local-sample-report)]
    (cond
      (.exists local)
      {:status 200 :body (edn/read-string (slurp local))}

      (io/resource "fixtures/reports/streetlight.edn")
      {:status 200 :body (update (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn")))
                                 :blocks into (local-blocks))}

      :else
      {:status 404 :body {:error "找不到範例報告；請以 :dev alias 啟動"}})))

(defn routes []
  [["/api"
    ["/health" {:get (fn [_] {:status 200 :body {:status "ok"}})}]
    ["/sample-report" {:get sample-report}]]])

(defn- revalidate
  "靜態檔每次都向伺服器確認是否更新（未變更時回 304），避免瀏覽器沿用舊的 CSS／JS。"
  [h]
  (fn [req]
    (some-> (h req) (assoc-in [:headers "Cache-Control"] "no-cache"))))

(defn handler []
  (ring/ring-handler
   (ring/router (routes)
                {:data {:muuntaja m/instance
                        :middleware [muuntaja/format-middleware]}})
   (ring/routes
    (revalidate (ring/create-resource-handler {:path "/" :root "public"}))
    (ring/create-default-handler))))

(defmethod ig/init-key :knot/http [_ {:keys [config]}]
  (let [port (:http/port config)]
    (println (str "Knot 伺服器：http://localhost:" port))
    (http/run-server (handler) {:port port :legacy-return-value? false})))

(defmethod ig/halt-key! :knot/http [_ server]
  ;; 已停止的伺服器 server-stop! 回傳 nil
  (some-> (http/server-stop! server) deref))
