(ns knot.seed
  "把範例報告（巢狀的卡片樹、短 id）匯入成一份報告。短 id 一律換成 UUID（SPEC 4：對外只用 UUID）。"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
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
  "以 fixture->state 的結果建立一份新報告，回傳 report id。"
  [store {:keys [report nodes relations blocks facts]}]
  (let [report-id (db/create-report! store report)]
    (db/apply-changes! (db/conn store report-id)
                       {:origin :import
                        :label "匯入範例報告"
                        :changes {:nodes {:upsert nodes}
                                  :relations {:upsert relations}
                                  :blocks {:upsert blocks}
                                  :facts {:upsert facts}}})
    report-id))

(defn seed!
  "以範例報告建立一份新報告，回傳 report id。"
  [store fx]
  (import-state! store (fixture->state fx)))

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
