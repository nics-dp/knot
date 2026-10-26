(ns knot.seed
  "把範例報告（巢狀的卡片樹、短 id）匯入空白報告。短 id 一律換成 UUID（SPEC 4：對外只用 UUID）。"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datalevin.core :as d]
            [knot.db :as db]
            [knot.tree :as tree]))

(defn fixture->state
  "回傳 {:report :nodes :relations :blocks :facts :selected}，id 都已換成 UUID。"
  [fx]
  (let [nodes (vals (tree/from-nested (:tree fx)))
        ids (into {} (map (fn [k] [k (random-uuid)]))
                  (concat (map :node/id nodes) (map :block/id (:blocks fx))))
        id #(when % (ids %))]
    {:report (select-keys fx [:report/title :report/subtitle :report/style-name])
     :selected (id (:ui/selected fx))
     :nodes (vec (for [n nodes]
                   (cond-> (assoc n :node/id (id (:node/id n)) :node/parent (id (:node/parent n)))
                     (:node/citations n) (update :node/citations
                                                 (fn [cs] (mapv #(update % :block id) cs))))))
     :relations (vec (for [r (:relations fx)]
                       (-> r (update :relation/from id) (update :relation/to id))))
     :blocks (vec (for [b (:blocks fx)] (update b :block/id id)))
     :facts (vec (for [f (:facts fx)]
                   (cond-> f (:fact/block f) (update :fact/block id))))}))

(defn import-state!
  "把 fixture->state 的結果匯入報告（報告必須是空的）。"
  [conn {:keys [report nodes relations blocks facts]}]
  (when-not (db/empty-report? (d/db conn))
    (throw (ex-info "報告已有內容，不能匯入範例" {})))
  (db/update-report! conn report)
  (db/apply-changes! conn
                     {:origin :import
                      :label "匯入範例報告"
                      :changes {:nodes {:upsert nodes}
                                :relations {:upsert relations}
                                :blocks {:upsert blocks}
                                :facts {:upsert facts}}}))

(defn seed!
  "把範例報告匯入空白報告。"
  [conn fx]
  (import-state! conn (fixture->state fx)))

(def local-sample
  "使用者在本機準備的測試報告（節錄自真實參考資料，不進 git）。"
  "local/sample-report.edn")

(defn sample-fixture
  "本機測試報告優先；沒有時用 test-resources 的虛構範例（只在 :dev／:test 的 classpath 上）。"
  []
  (let [local (io/file local-sample)]
    (cond
      (.exists local) (edn/read-string (slurp local))
      (io/resource "fixtures/reports/streetlight.edn") (edn/read-string (slurp (io/resource "fixtures/reports/streetlight.edn"))))))
