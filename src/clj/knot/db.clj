(ns knot.db
  "報告資料庫（SPEC 4）。一個報告目錄就是一份報告，只有一個 Datalevin 資料庫；entity id 不離開本命名空間，
  對外一律用前端的實體形式（卡片、關係、素材、事實），以 UUID 或 key 識別。

  寫入一律經 apply-changes!：一次呼叫是一個修改組，每個實體記下修改前後的快照；
  復原是新增一個反向的修改組，不刪除歷史。"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datalevin.core :as d]
            [jsonista.core :as json]
            [knot.history :as history]
            [knot.order-key :as ok]
            [knot.tree :as tree]))

;; ---------------------------------------------------------------------------
;; Schema

(defn- id-attr [] {:db/valueType :db.type/uuid :db/unique :db.unique/identity})
(def ^:private string {:db/valueType :db.type/string})
(def ^:private kw {:db/valueType :db.type/keyword})
(def ^:private ref-one {:db/valueType :db.type/ref})
(def ^:private refs {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many})
(def ^:private bool {:db/valueType :db.type/boolean})
(def ^:private long* {:db/valueType :db.type/long})

(def schema
  {;; 報告
   :report/id (id-attr)
   :report/title string
   :report/subtitle string
   :report/style-name string
   :report/style-snapshot string
   :report/style-source {:db/valueType :db.type/uuid}

   ;; 卡片
   :node/id (id-attr)
   :node/kind kw
   :node/parent ref-one
   :node/order-key string
   :node/name string
   :node/title string
   :node/draft (assoc string :db/fulltext true)
   :node/prose string
   :node/status kw
   :node/version long*
   :node/target-chars long*
   :node/appendix bool
   :node/label (assoc string :db/unique :db.unique/value)
   :node/facts refs
   :node/rows refs
   :node/table ref-one
   :node/table-view string
   :node/table-content string ; 表格卡片自己的內容（EDN），前端形式為 {:columns [...] :rows [[...]]}
   :node/figure-file string
   :node/caption string

   ;; 段落關係
   :relation/id (id-attr)
   :relation/key (assoc string :db/unique :db.unique/identity)
   :relation/from ref-one
   :relation/to ref-one
   :relation/type kw
   :relation/remote bool
   :relation/pending bool

   ;; 素材
   :source/id (id-attr)
   :source/file-name (assoc string :db/unique :db.unique/identity)
   :source/path string
   :source/kind kw
   :block/id (id-attr)
   :block/source ref-one
   :block/kind kw
   :block/title string
   :block/text (assoc string :db/fulltext true)
   :block/location string
   :block/status kw
   :block/note string
   :block/ai-split bool ; 舊欄位：不再使用，保留 schema 以免舊資料出錯
   :block/table ref-one

   ;; 表格與事實
   :table/id (id-attr)
   :table/name string
   :table/kind kw
   :table/columns string
   :table/name-column string
   :row/id (id-attr)
   :row/table ref-one
   :row/order-key string
   :row/cells string
   :fact/key (assoc string :db/unique :db.unique/identity)
   :fact/label string
   :fact/value string
   :fact/unit string
   :fact/row ref-one ; 舊欄位：不再使用；指向儲存格的列改存 :fact/row-index
   :fact/row-index long* ; 0 起算的列索引（前端形式的 :fact/row）
   :fact/column string
   :fact/block ref-one

   ;; 風格（SPEC 5.7）：整份報告一份，識別鍵固定為 "report"
   :style/key (assoc string :db/unique :db.unique/identity)
   :style/text string

   ;; 引用
   :citation/id (id-attr)
   :citation/node ref-one
   :citation/block ref-one
   :citation/row ref-one
   :citation/where string
   :citation/order long*

   ;; 修改組與修訂
   :changeset/id (id-attr)
   :changeset/origin kw
   :changeset/by kw
   :changeset/at {:db/valueType :db.type/instant}
   :changeset/label string
   :changeset/undoes ref-one
   ;; 修改組分組（B1）：agent 一次工作的多次寫入共用 group-id，歷程與復原都當一次
   :changeset/group string
   :changeset/group-id {:db/valueType :db.type/uuid}
   :changeset/undoes-group {:db/valueType :db.type/uuid}
   :revision/changeset ref-one
   :revision/collection kw
   :revision/target string
   :revision/node ref-one
   :revision/version long*
   :revision/kind kw
   :revision/before string
   :revision/after string

   ;; 留言與待辦（SPEC 4、5.11）：不屬於修改組
   :note/id (id-attr)
   :note/node {:db/valueType :db.type/uuid}
   :note/author kw
   :note/text string
   :note/todo bool
   :note/state kw
   :note/reply-to {:db/valueType :db.type/uuid}
   :note/at {:db/valueType :db.type/instant}
   :note/done-at {:db/valueType :db.type/instant}
   :note/changeset {:db/valueType :db.type/uuid}
   ;; Knot 為相關段落留下的待辦（5.6）：觸發的卡片與依賴類型
   :note/trigger {:db/valueType :db.type/uuid}   ; 舊欄位：讀取時轉成 :note/triggers 的一個元素
   :note/dependency kw
   ;; 觸發的段落們：[{:id uuid :dependency :summary|:argument :reason string :fields #{:draft :prose}}]，以 pr-str 存成字串
   :note/triggers string
   :note/updated-at {:db/valueType :db.type/instant}}) ; 最後一次附加觸發的時間

;; ---------------------------------------------------------------------------
;; Datalog 規則

(def rules
  "樹與依賴的規則（SPEC 4、5.6）。"
  '[;; ?a 是 ?d 的祖先
    [(ancestor ?a ?d) [?d :node/parent ?a]]
    [(ancestor ?a ?d) [?d :node/parent ?p] (ancestor ?a ?p)]

    ;; 摘要依賴：以摘要關係指向 ?n 的段落
    [(summary-dependent ?n ?dep)
     [?r :relation/to ?n] [?r :relation/type :summarizes] [?r :relation/from ?dep]]
    ;; 摘要依賴：?n 所有上層標題的引言（隱含，不另存關係）
    [(summary-dependent ?n ?dep)
     (ancestor ?h ?n) [?dep :node/parent ?h] [?dep :node/kind :intro]]

    ;; 論證依賴：下一段（相鄰關係為因果或遞進）
    [(argument-dependent ?n ?dep)
     [?r :relation/from ?n] [?r :relation/to ?dep]
     [(missing? $ ?r :relation/remote)]
     [?r :relation/type ?t] [(contains? #{:causal :progression} ?t)]]
    ;; 論證依賴：以「依據」關係指向 ?n 的段落
    [(argument-dependent ?n ?dep)
     [?r :relation/to ?n] [?r :relation/type :basis] [?r :relation/from ?dep]]])

(defn dependents
  "受 node-id 影響的段落：{:summary #{id} :argument #{id}}。"
  [db node-id]
  (let [q (fn [rule]
            (set (d/q [:find '[?id ...]
                       :in '$ '% '?nid
                       :where '[?n :node/id ?nid] (list rule '?n '?dep) '[?dep :node/id ?id]]
                      db rules node-id)))]
    {:summary (q 'summary-dependent)
     :argument (q 'argument-dependent)}))

(defn ancestor-ids [db node-id]
  (set (d/q '[:find [?id ...] :in $ % ?nid
              :where [?n :node/id ?nid] (ancestor ?a ?n) [?a :node/id ?id]]
            db rules node-id)))

;; ---------------------------------------------------------------------------
;; 連線：一個報告目錄就是一份報告，資料庫在 <報告目錄>/knot-data/db
;; （0.1.0 為隱藏的 .knot/db，開啟時自動改名）

(def data-dir-name "knot-data")

(defn db-dir [report-dir]
  (io/file report-dir data-dir-name "db"))

(defn- migrate-hidden-dir!
  "0.1.0 的資料在 <報告目錄>/.knot；還沒有 knot-data 時改名過去。回傳是否改名。"
  [report-dir]
  (let [old (io/file report-dir ".knot")
        new (io/file report-dir data-dir-name)]
    (when (and (.isDirectory old) (not (.exists new)))
      (java.nio.file.Files/move (.toPath old) (.toPath new)
                                (make-array java.nio.file.CopyOption 0))
      true)))

(defn report-info [db]
  (d/q '[:find (pull ?r [:report/id :report/title :report/subtitle :report/style-name]) .
         :where [?r :report/id]]
       db))

(defn open-report
  "開啟報告目錄中的報告；還沒有報告時建立一份空白報告，名稱取自目錄名稱。舊版的 .knot/ 先改名為 knot-data/。回傳連線。"
  [report-dir]
  (let [_ (migrate-hidden-dir! report-dir)
        dir (db-dir report-dir)
        _ (.mkdirs dir)
        c (d/get-conn (str dir) schema)]
    (when-not (report-info (d/db c))
      (d/transact! c [{:report/id (random-uuid)
                       :report/title (.getName (.getCanonicalFile (io/file report-dir)))}]))
    c))

(defn close-report [conn]
  (d/close conn))

(defn update-report!
  "修改報告的名稱等資訊（不屬於修改組）。值為 nil 的欄位表示清除。"
  [conn info]
  (let [id (:report/id (report-info (d/db conn)))
        info (select-keys info [:report/title :report/subtitle :report/style-name])]
    (d/transact! conn (into [(merge (into {} (filter (comp some? val)) info) {:report/id id})]
                            (for [[k v] info :when (nil? v)] [:db/retract [:report/id id] k])))))

(defn empty-report? [db]
  (empty? (d/q '[:find [?e ...] :where [?e :node/id]] db)))

;; ---------------------------------------------------------------------------
;; 讀取：資料庫 → 前端形式

(def ^:private node-scalars
  [:node/kind :node/order-key :node/name :node/title :node/draft :node/status
   :node/label :node/figure-file :node/caption :node/table-view :node/target-chars :node/appendix])

(defn- read-json [s]
  (when s (json/read-value s json/keyword-keys-object-mapper)))

(defn- write-json [x]
  (when x (json/write-value-as-string x)))

(defn- read-edn [s]
  (when s (edn/read-string s)))

(defn- citations-by-node [db]
  (->> (d/q '[:find ?nid ?bid ?where ?order
              :where
              [?c :citation/node ?n] [?n :node/id ?nid]
              [?c :citation/block ?b] [?b :block/id ?bid]
              [(get-else $ ?c :citation/order 0) ?order]
              [(get-else $ ?c :citation/where "") ?where]]
            db)
       (group-by first)
       (into {} (map (fn [[nid rows]]
                       [nid (->> rows
                                 (sort-by peek)
                                 (mapv (fn [[_ bid where]]
                                         (cond-> {:block bid}
                                           (seq where) (assoc :where where)))))])))))

(def ^:private node-pattern
  (into [:node/id :node/prose :node/table-content :node/version
         {:node/parent [:node/id]}]
        node-scalars))

(defn- node->client [citations e]
  (let [id (:node/id e)
        cites (get citations id)]
    (cond-> (select-keys e (concat [:node/id :node/version] node-scalars))
      true (assoc :node/parent (get-in e [:node/parent :node/id]))
      (:node/prose e) (assoc :node/prose (read-json (:node/prose e)))
      (:node/table-content e) (assoc :node/table-content (read-edn (:node/table-content e)))
      (seq cites) (assoc :node/citations cites))))

(defn- pull-all [db attr pattern]
  (d/q [:find [(list 'pull '?e pattern) '...] :where ['?e attr]] db))

(defn- read-nodes [db]
  (let [citations (citations-by-node db)]
    (mapv #(node->client citations %) (pull-all db :node/id node-pattern))))

(defn- relation->client [e]
  (cond-> {:relation/from (get-in e [:relation/from :node/id])
           :relation/to (get-in e [:relation/to :node/id])
           :relation/type (:relation/type e)}
    (:relation/remote e) (assoc :relation/remote true)
    (some? (:relation/pending e)) (assoc :relation/pending (:relation/pending e))))

(def ^:private relation-pattern
  [:relation/key :relation/type :relation/remote :relation/pending
   {:relation/from [:node/id]} {:relation/to [:node/id]}])

(defn- read-relations [db]
  (->> (pull-all db :relation/key relation-pattern)
       (sort-by :relation/key)
       (mapv relation->client)))

(defn- table-rows [db table-eid columns]
  (->> (d/q '[:find ?k ?cells :in $ ?t :where [?r :row/table ?t] [?r :row/order-key ?k] [?r :row/cells ?cells]]
            db table-eid)
       (sort-by first)
       (mapv (fn [[_ cells]]
               (let [m (read-edn cells)]
                 (mapv #(get m (:col/id %) "") columns))))))

(def ^:private block-pattern
  [:db/id :block/id :block/kind :block/title :block/text :block/location :block/status :block/note
   {:block/source [:source/file-name]}
   {:block/table [:db/id :table/kind :table/columns]}])

(defn- block->client [db e]
  (let [table (:block/table e)
        columns (some-> (:table/columns table) read-edn)]
    (cond-> (-> (select-keys e [:block/id :block/kind :block/title :block/text :block/location
                                :block/status :block/note])
                (assoc :block/source (get-in e [:block/source :source/file-name]))
                ;; 舊資料的 :candidate（候選）一律視為可引用；:block/ai-split 不再使用，不讀出
                (update :block/status #(if (= :candidate %) :confirmed %)))
      (:table/kind table) (assoc :table/kind (:table/kind table))
      columns (assoc :table/columns (mapv :col/name columns)
                     :table/rows (table-rows db (:db/id table) columns)))))

(defn- read-blocks [db]
  (->> (pull-all db :block/id block-pattern)
       (mapv #(block->client db %))
       (sort-by #(str (:block/source %) " " (:block/location %) " " (:block/title %)))
       vec))

(def style-key "風格的識別鍵（整份報告只有一份）。" "report")

(defn- read-facts [db]
  (->> (pull-all db :fact/key [:fact/key :fact/label :fact/value :fact/unit :fact/row-index :fact/column
                               {:fact/block [:block/id]}])
       (mapv (fn [e]
               ;; 前端形式的 :fact/row 是 0 起算的列索引，資料庫存在 :fact/row-index
               (cond-> (dissoc e :fact/block :fact/row-index)
                 (:fact/block e) (assoc :fact/block (get-in e [:fact/block :block/id]))
                 (some? (:fact/row-index e)) (assoc :fact/row (:fact/row-index e)))))
       (sort-by :fact/key)
       vec))

(defn read-style
  "整份報告的風格：{:style/key \"report\" :style/text ...}；沒有設定時為 nil。"
  [db]
  (when-let [e (d/pull db [:style/key :style/text] [:style/key style-key])]
    (when (:style/key e) (update e :style/text #(or % "")))))

(def ^:private note-attrs
  [:note/id :note/node :note/author :note/text :note/todo :note/state
   :note/reply-to :note/at :note/done-at :note/changeset :note/trigger :note/dependency
   :note/triggers :note/updated-at])

(declare content-modified-since?)

(defn- note->client
  "Knot 待辦：:note/triggers 讀成向量（舊的單一 :note/trigger 轉成一個元素）；
  未完成的 Knot 待辦在目標段落之後又被修改意圖或本文時加 :note/modified-after true（算出來的，不存）。"
  [db n]
  (let [triggers (cond (:note/triggers n) (read-edn (:note/triggers n))
                       (:note/trigger n) [(cond-> {:id (:note/trigger n)}
                                            (:note/dependency n) (assoc :dependency (:note/dependency n)))])
        n (cond-> (dissoc n :db/id :note/trigger :note/dependency)
            triggers (assoc :note/triggers (vec triggers)))]
    (if (and (= :knot (:note/author n)) (= :open (:note/state n)) (:note/node n)
             (content-modified-since? db (:note/node n) (or (:note/updated-at n) (:note/at n))))
      (assoc n :note/modified-after true)
      n)))

(defn list-notes
  "所有留言（含已隱藏的：所屬卡片已刪除者也在內），依時間由舊到新；
  前端形式為 :note/* 的 map，沒有值的 key 省略。"
  [db]
  (->> (pull-all db :note/id (into [:db/id] note-attrs))
       (sort-by (juxt :note/at :db/id))
       (mapv #(note->client db %))))

(declare style-info)

(defn load-state
  "整份報告的前端形式：{:report :nodes :relations :blocks :facts :notes}。"
  [db]
  {:report (dissoc (report-info db) :db/id)
   :nodes (read-nodes db)
   :relations (read-relations db)
   :blocks (read-blocks db)
   :facts (read-facts db)
   :style (read-style db)
   :style-info (some-> (style-info db) (dissoc :text :chars))
   :notes (list-notes db)})

(defn- entity-before
  "某集合中識別鍵為 k 的實體，前端形式；不存在時為 nil。"
  [db coll k]
  (case coll
    :nodes (when-let [e (d/pull db node-pattern [:node/id k])]
             (when (:node/id e) (node->client (citations-by-node db) e)))
    :relations (when-let [e (d/pull db relation-pattern [:relation/key k])]
                 (when (:relation/key e) (relation->client e)))
    :blocks (when-let [e (d/pull db block-pattern [:block/id k])]
              (when (:block/id e) (block->client db e)))
    :facts (some #(when (= k (:fact/key %)) %) (read-facts db))
    :style (let [s (read-style db)] (when (= k (:style/key s)) s))))

;; ---------------------------------------------------------------------------
;; 寫入：前端形式 → 交易

(defn- exists? [db attr v]
  (some? (d/entid db [attr v])))

(defn- prose-fact-keys [prose]
  (->> (tree-seq coll? seq prose)
       (keep #(when (and (map? %) (= "fact" (:type %))) (get-in % [:attrs :key])))
       distinct))

(defn- node-tx
  "卡片的交易資料。ref 把 [類型 識別鍵] 轉成這次交易可用的參照（同一交易新增的實體用 tempid）。"
  [db ref before node]
  (let [;; 鎖定功能已移除；復原到舊快照時，鎖定的段落視為人工改過
        node (cond-> node (= :locked (:node/status node)) (assoc :node/status :human))
        id (:node/id node)
        eid [:node/id id]
        existed? (some? before)
        ;; 任何欄位（含位置、狀態、依據）變動都加一，版本檢查才能擋下覆蓋移動的寫入
        version (if (= (dissoc before :node/version) (dissoc node :node/version))
                  (:node/version before 1)
                  (inc (or (:node/version before) 0)))
        removed (for [a (concat node-scalars [:node/prose :node/table-content :node/parent])
                      :when (and existed? (some? (get before a)) (nil? (get node a)))]
                  [:db/retract eid a])
        old-cites (when existed?
                    (d/q '[:find [?c ...] :in $ ?nid :where [?n :node/id ?nid] [?c :citation/node ?n]] db id))
        fact-keys (filter #(ref :fact %) (prose-fact-keys (:node/prose node)))]
    (concat
     ;; 清掉多值參照，下面重新加入
     (when existed?
       [[:db/retract eid :node/facts]])
     removed
     (for [c old-cites] [:db/retractEntity c])
     [(cond-> (merge {:db/id (ref :node id) :node/id id :node/version version}
                     (into {} (keep (fn [a] (when-some [v (get node a)] [a v]))) node-scalars))
        (:node/parent node) (assoc :node/parent (ref :node (:node/parent node)))
        (:node/prose node) (assoc :node/prose (write-json (:node/prose node)))
        (:node/table-content node) (assoc :node/table-content (pr-str (:node/table-content node)))
        (seq fact-keys) (assoc :node/facts (mapv #(ref :fact %) fact-keys)))]
     (for [[i {:keys [block where]}] (map-indexed vector (:node/citations node))
           :let [b (ref :block block)]
           :when b]
       (cond-> {:citation/id (random-uuid)
                :citation/node (ref :node id)
                :citation/block b
                :citation/order i}
         (seq where) (assoc :citation/where where))))))

(defn- relation-tx [ref before r]
  (let [k (tree/relation-key r)
        eid [:relation/key k]]
    (concat
     (when before
       (for [a [:relation/type :relation/remote :relation/pending]
             :when (and (some? (get before a)) (nil? (get r a)))]
         [:db/retract eid a]))
     [(cond-> {:relation/key k
               :relation/from (ref :node (:relation/from r))
               :relation/to (ref :node (:relation/to r))}
        (not before) (assoc :relation/id (random-uuid))
        (:relation/type r) (assoc :relation/type (:relation/type r))
        (:relation/remote r) (assoc :relation/remote true)
        (some? (:relation/pending r)) (assoc :relation/pending (:relation/pending r)))])))

(defn- columns-of [names]
  (vec (map-indexed (fn [i n] {:col/id (str "c" i) :col/name n :col/type :text}) names)))

(defn- block-tx [db ref before b]
  (let [id (:block/id b)
        eid [:block/id id]
        tid (str "table-" id)
        table? (seq (:table/columns b))
        table-changed? (not= (select-keys before [:table/kind :table/columns :table/rows])
                             (select-keys b [:table/kind :table/columns :table/rows]))
        old-table (when before (:db/id (:block/table (d/pull db [{:block/table [:db/id]}] eid))))]
    (concat
     (when before
       (for [a [:block/title :block/text :block/location :block/status :block/note]
             :when (and (some? (get before a)) (nil? (get b a)))]
         [:db/retract eid a]))
     (when (and old-table table-changed?)
       (concat
        (for [r (d/q '[:find [?r ...] :in $ ?t :where [?r :row/table ?t]] db old-table)]
          [:db/retractEntity r])
        [[:db/retractEntity old-table]]))
     (when-let [src (:block/source b)]
       [(cond-> {:db/id (str "source-" src) :source/file-name src}
          (not (exists? db :source/file-name src)) (assoc :source/id (random-uuid)))])
     [(cond-> (into {:db/id (ref :block id) :block/id id}
                    (keep (fn [a] (when-some [v (get b a)] [a v])))
                    [:block/kind :block/title :block/text :block/location :block/status :block/note])
        (:block/source b) (assoc :block/source (str "source-" (:block/source b)))
        (and table? (or table-changed? (not old-table))) (assoc :block/table tid))]
     (when (and table? (or table-changed? (not old-table)))
       (let [cols (columns-of (:table/columns b))]
         (cons (cond-> {:db/id tid :table/id (random-uuid) :table/name (or (:block/title b) "")
                        :table/columns (pr-str cols)}
                 (:table/kind b) (assoc :table/kind (:table/kind b)))
               (map (fn [row k]
                      {:row/id (random-uuid) :row/table tid :row/order-key k
                       :row/cells (pr-str (zipmap (map :col/id cols) row))})
                    (:table/rows b)
                    (ok/initial-keys (count (:table/rows b))))))))))

(defn- fact-tx [ref before f]
  (let [eid [:fact/key (:fact/key f)]]
    (concat
     (when before
       (for [a [:fact/label :fact/value :fact/unit :fact/column :fact/block :fact/row]
             :when (and (some? (get before a)) (nil? (get f a)))]
         [:db/retract eid (if (= a :fact/row) :fact/row-index a)]))
     [(cond-> (into {:db/id (ref :fact (:fact/key f)) :fact/key (:fact/key f)}
                    (keep (fn [a] (when-some [v (get f a)] [a v])))
                    [:fact/label :fact/value :fact/unit :fact/column])
        (some? (:fact/row f)) (assoc :fact/row-index (:fact/row f))
        (and (:fact/block f) (ref :block (:fact/block f))) (assoc :fact/block (ref :block (:fact/block f))))])))

(defn- style-tx [before s]
  (concat
   (when (and before (nil? (:style/text s)))
     [[:db/retract [:style/key (:style/key s)] :style/text]])
   [(cond-> {:style/key (:style/key s)}
      (some? (:style/text s)) (assoc :style/text (:style/text s)))]))

(defn- delete-tx [db coll k]
  (case coll
    :nodes (concat
            (for [c (d/q '[:find [?c ...] :in $ ?nid :where [?n :node/id ?nid] [?c :citation/node ?n]] db k)]
              [:db/retractEntity c])
            [[:db/retractEntity [:node/id k]]])
    :relations [[:db/retractEntity [:relation/key k]]]
    :blocks (let [t (:db/id (:block/table (d/pull db [{:block/table [:db/id]}] [:block/id k])))]
              (concat
               (for [c (d/q '[:find [?c ...] :in $ ?bid :where [?b :block/id ?bid] [?c :citation/block ?b]] db k)]
                 [:db/retractEntity c])
               (when t
                 (concat (for [r (d/q '[:find [?r ...] :in $ ?t :where [?r :row/table ?t]] db t)]
                           [:db/retractEntity r])
                         [[:db/retractEntity t]]))
               [[:db/retractEntity [:block/id k]]]))
    :facts [[:db/retractEntity [:fact/key k]]]
    :style [[:db/retractEntity [:style/key k]]]))

(def ^:private collection-order
  "交易內的順序：素材與事實先於卡片（卡片引用它們），關係最後。"
  [:blocks :facts :style :nodes :relations])

(def ^:private key-fns
  {:nodes :node/id :relations tree/relation-key :blocks :block/id :facts :fact/key :style :style/key})

(defn- version-conflicts
  "[:nodes :upsert] 中已存在的卡片，送來的 :node/version 與資料庫目前的不同者。"
  [db changes]
  (vec (for [n (get-in changes [:nodes :upsert])
             :let [actual (:node/version (d/pull db [:node/version] [:node/id (:node/id n)]))]
             :when (and actual (not= actual (:node/version n)))]
         {:node/id (:node/id n) :expected (:node/version n) :actual actual})))

(def group-gap-ms
  "同一組的寫入相隔不超過這段時間才算同一次修改（B1）。"
  (* 30 60 1000))

(defn undone-ids
  "已被復原的修改組 id 集合：被復原修改組的 :changeset/undoes 指到的，
  加上被整組復原（:changeset/undoes-group）的組內所有修改組。"
  [db]
  (into (set (d/q '[:find [?t ...] :where [_ :changeset/undoes ?u] [?u :changeset/id ?t]] db))
        (d/q '[:find [?t ...] :where [_ :changeset/undoes-group ?g] [?c :changeset/group-id ?g] [?c :changeset/id ?t]] db)))

(defn- continuing-group-id
  "這次寫入的 group-id：資料庫中最近一個修改組（依時間，不分來源）同組名、同為 agent、
  相隔不超過 group-gap-ms 且未被復原時沿用它的 group-id，否則產生新的。"
  [db group by]
  (let [[_ at id g gid by'] (->> (d/q '[:find ?c ?at ?id ?g ?gid ?by
                                       :where [?c :changeset/id ?id] [?c :changeset/at ?at]
                                       [(get-else $ ?c :changeset/group "") ?g]
                                       [(get-else $ ?c :changeset/group-id "") ?gid]
                                       [(get-else $ ?c :changeset/by :user) ?by]]
                                     db)
                                (sort-by (juxt second first))
                                last)]
    (if (and gid (not= "" gid) (= group g) (= :agent by) (= :agent by')
             (<= (- (System/currentTimeMillis) (inst-ms at)) group-gap-ms)
             (not (contains? (undone-ids db) id)))
      gid
      (random-uuid))))

(defn- write-changes!
  "apply-changes! 的主體；呼叫端須持有 conn 的鎖。"
  [conn {:keys [origin by label changes undoes undoes-group group]}]
  (let [db (d/db conn)
        upserted {:node (set (map :node/id (get-in changes [:nodes :upsert])))
                  :block (set (map :block/id (get-in changes [:blocks :upsert])))
                  :fact (set (map :fact/key (get-in changes [:facts :upsert])))}
        attr {:node :node/id :block :block/id :fact :fact/key}
        ;; 同一交易新增或更新的實體用 tempid；其他用 lookup ref（不存在時為 nil）
        ref (fn [kind k]
              (when (some? k)
                (cond
                  (contains? (upserted kind) k) (str (name kind) "-" k)
                  (exists? db (attr kind) k) [(attr kind) k])))
        cs-id (random-uuid)
        group-id (when-not (str/blank? group) (continuing-group-id db group (or by :user)))
        revisions (atom [])
        revise! (fn [coll k before after]
                  (swap! revisions conj
                         (cond-> {:revision/changeset "changeset"
                                  :revision/collection coll
                                  :revision/target (str (name coll) "/" k)
                                  :revision/after (pr-str after)}
                           before (assoc :revision/before (pr-str before))
                           (and (= coll :nodes) after) (assoc :revision/node (ref :node k)))))
        tx (vec
            (mapcat
             (fn [coll]
               (let [{:keys [upsert delete]} (get changes coll)
                     key-fn (key-fns coll)]
                 (concat
                  (mapcat (fn [e]
                            (let [k (key-fn e)
                                  before (entity-before db coll k)]
                              (when (not= before e)
                                (revise! coll k before e)
                                (case coll
                                  :nodes (node-tx db ref before e)
                                  :relations (relation-tx ref before e)
                                  :blocks (block-tx db ref before e)
                                  :facts (fact-tx ref before e)
                                  :style (style-tx before e)))))
                          upsert)
                  (mapcat (fn [k]
                            (when-let [before (entity-before db coll k)]
                              (revise! coll k before nil)
                              (delete-tx db coll k)))
                          delete))))
             collection-order))
        written? (seq @revisions)
        db-after (if written?
                   (:db-after (d/transact! conn (concat tx
                                                        [(cond-> {:db/id "changeset"
                                                                  :changeset/id cs-id
                                                                  :changeset/origin (or origin :manual)
                                                                  :changeset/by (or by :user)
                                                                  :changeset/at (java.util.Date.)
                                                                  :changeset/label (or label "")}
                                                           undoes (assoc :changeset/undoes [:changeset/id undoes])
                                                           undoes-group (assoc :changeset/undoes-group undoes-group)
                                                           group-id (assoc :changeset/group group :changeset/group-id group-id))]
                                                        @revisions)))
                   db)]
    (cond-> {:versions (into {}
                             (keep (fn [n]
                                     (let [id (:node/id n)]
                                       (when-let [v (:node/version (d/pull db-after [:node/version] [:node/id id]))]
                                         [id v]))))
                             (get-in changes [:nodes :upsert]))}
      written? (assoc :changeset/id cs-id)
      (and written? group-id) (assoc :changeset/group-id group-id))))

(defn apply-changes!
  "把前端的變動（knot.sync/diff 的格式）寫成一個修改組。
  選項：:by（:user 預設或 :agent）、:check-versions?（見下）、:undoes（復原用）。
  回傳 {:changeset/id uuid :changeset/group-id（屬於 group 時）:versions {卡片 id 寫入後的版本}}；沒有實際變動時沒有 :changeset/id。
  :check-versions? 為 true 時，已存在的卡片若送來的 :node/version 與資料庫目前的不同，
  就不寫入任何東西，回傳 {:conflicts [{:node/id .. :expected 送來的 :actual 目前的}]}。
  檢查與寫入在同一把鎖內，兩個請求不會同時通過檢查。changes 為空時回傳 nil。"
  [conn {:keys [changes check-versions?] :as opts}]
  (when (seq changes)
    (locking conn
      (let [conflicts (when check-versions? (seq (version-conflicts (d/db conn) changes)))]
        (if conflicts
          {:conflicts (vec conflicts)}
          (write-changes! conn opts))))))

;; ---------------------------------------------------------------------------
;; 風格的最後修改（SPEC 5.7）

(defn style-info
  "風格與它的最後修改：{:text :chars :updated-at :updated-by}；沒有風格時為 nil。
  最後修改取自最近一個含風格修訂的修改組（復原也算，由使用者做的）。"
  [db]
  (when-let [{:style/keys [text]} (read-style db)]
    (let [[at by] (->> (d/q '[:find ?at ?by
                              :where [?r :revision/collection :style] [?r :revision/changeset ?c]
                              [?c :changeset/at ?at]
                              [(get-else $ ?c :changeset/by :user) ?by]]
                            db)
                       (sort-by first)
                       last)]
      (cond-> {:text text :chars (count text)}
        at (assoc :updated-at at :updated-by by)))))

;; ---------------------------------------------------------------------------
;; 復原

(defn- undoable-changeset
  "最近一個尚未復原、也不是復原本身的修改組：[id at label group-id group]；沒有 group 時後兩者為 \"\"。"
  [db]
  (let [undone (undone-ids db)]
    ;; 同一毫秒內寫入的修改組以實體 id（建立順序）區分先後
    (->> (d/q '[:find ?id ?at ?label ?gid ?group ?c :where
                [?c :changeset/id ?id] [?c :changeset/at ?at] [?c :changeset/origin ?o]
                [(not= ?o :undo)] [(not= ?o :import)]
                [(get-else $ ?c :changeset/label "") ?label]
                [(get-else $ ?c :changeset/group-id "") ?gid]
                [(get-else $ ?c :changeset/group "") ?group]]
              db)
         (remove (comp undone first))
         (sort-by (juxt second peek))
         last
         pop)))

(defn- group-members
  "同一個 group-id 中尚未復原的修改組 id，舊到新。"
  [db gid]
  (let [undone (undone-ids db)]
    (->> (d/q '[:find ?id ?at ?c :in $ ?gid
                :where [?c :changeset/group-id ?gid] [?c :changeset/id ?id] [?c :changeset/at ?at]]
              db gid)
         (remove (comp undone first))
         (sort-by (juxt second #(nth % 2)))
         (mapv first))))

(defn- earliest-befores
  "這些修改組的修訂中，每個實體取最早那筆的 before：[[coll target before] ...]（before 為 \"\" 表示當時不存在）。"
  [db cs-ids]
  (->> (d/q '[:find ?cat ?c ?coll ?target ?before
              :in $ [?cid ...]
              :where [?c :changeset/id ?cid] [?c :changeset/at ?cat] [?r :revision/changeset ?c]
              [?r :revision/collection ?coll] [?r :revision/target ?target]
              [(get-else $ ?r :revision/before "") ?before]]
            db cs-ids)
       (sort-by (juxt first second))
       (reduce (fn [acc [_ _ coll target before]]
                 (if (contains? acc [coll target]) acc (assoc acc [coll target] before)))
               {})
       (mapv (fn [[[coll target] before]] [coll target before]))))

(defn undo!
  "復原最近一個修改組：新增一個把各實體還原為修改前快照的修改組。
  該修改組屬於某個 group（agent 一次工作的多次寫入）時整組一起復原：每個實體還原為組內最早一筆的修改前快照，
  只新增一個復原修改組。回傳被復原的 label（整組時為組名）；沒有可復原的時為 nil。"
  [conn]
  (locking conn
    (when-let [[cs-id _ label gid group] (undoable-changeset (d/db conn))]
      (let [db (d/db conn)
            grouped? (not= "" gid)
            members (if grouped? (group-members db gid) [cs-id])
            label (if grouped? group label)
            changes (reduce (fn [acc [coll target before]]
                              (let [k (let [s (subs target (inc (count (name coll))))]
                                        (if (#{:nodes :blocks} coll) (parse-uuid s) s))]
                                (if (str/blank? before)
                                  (update-in acc [coll :delete] (fnil conj []) k)
                                  (update-in acc [coll :upsert] (fnil conj []) (read-edn before)))))
                            {} (earliest-befores db members))]
        (apply-changes! conn (cond-> {:origin :undo :label (str "復原：" label) :changes changes
                                      :undoes (last members)}
                               grouped? (assoc :undoes-group gid)))
        label))))

;; ---------------------------------------------------------------------------
;; 卡片的前一個版本

(def ^:private previous-fields
  "只保留本文的前一版；意圖不需要歷程。"
  [:node/prose])

(defn- node-revisions
  "某張卡片的修訂，新到舊：[{:at :label :before :after}]。"
  [db node-id]
  (->> (d/q '[:find ?at ?label ?before ?after
              :in $ ?target
              :where [?r :revision/target ?target] [?r :revision/changeset ?c]
              [?c :changeset/at ?at]
              [(get-else $ ?c :changeset/label "") ?label]
              [(get-else $ ?r :revision/before "") ?before]
              [?r :revision/after ?after]]
            db (str "nodes/" node-id))
       (map (fn [[at label before after]]
              {:at at :label label
               :before (or (some-> (not-empty before) read-edn) {})
               :after (or (read-edn after) {})}))
       (sort-by :at #(compare %2 %1))))

(defn content-modified-since?
  "卡片的意圖或本文在 since 之後被修改過（建立卡片不算）。"
  [db node-id since]
  (boolean (some (fn [{:keys [at before after]}]
                   (and (pos? (compare at since)) (seq before)
                        (or (not= (:node/draft before) (:node/draft after))
                            (not= (:node/prose before) (:node/prose after)))))
                 (node-revisions db node-id))))

(defn- field-previous [revs field]
  ;; 建立卡片（匯入、新增）的修訂沒有修改前的內容，不算一次修改
  (let [changed? #(and (seq (:before %))
                       (not= (get-in % [:before field]) (get-in % [:after field])))
        [latest & older] (drop-while (complement changed?) revs)
        typing? #(= "編輯文字" (:label %))]
    (when latest
      ;; 逐字輸入每停頓一次就是一個修改組；把連續的輸入合併回這次修改開始前
      (let [start (reduce (fn [newer r]
                            (if (and (typing? newer) (typing? r) (changed? r)
                                     (< (- (inst-ms (:at newer)) (inst-ms (:at r))) history/session-gap-ms))
                              r
                              (reduced newer)))
                          latest older)]
        {:value (get-in start [:before field]) :at (:at latest) :label (:label latest)}))))

(defn node-previous
  "卡片本文在最近一次修改之前的內容：{:node/prose {:value :at :label}}；沒有修改過時為空 map。"
  [db node-id]
  (let [revs (node-revisions db node-id)]
    (into {} (keep (fn [f] (some->> (field-previous revs f) (vector f)))) previous-fields)))

;; ---------------------------------------------------------------------------
;; 最近修改的卡片（SPEC 5.13）

(defn recent-cards
  "最近修改的卡片（不分 agent 或使用者），新到舊、每張卡片只列最近一次：
  [{:node/id :label :at :by}]，最多 n 張。匯入與復原本身不算修改；
  已刪除的卡片與已復原的修改組不列。"
  [db n]
  (let [exists? (set (d/q '[:find [?id ...] :where [_ :node/id ?id]] db))
        undone (undone-ids db)]
    (->> (d/q '[:find ?target ?at ?cl ?by ?cid ?group
                :where [?c :changeset/id ?cid] [?c :changeset/at ?at] [?c :changeset/origin ?o]
                [(not= ?o :undo)] [(not= ?o :import)]
                [(get-else $ ?c :changeset/by :user) ?by]
                [(get-else $ ?c :changeset/label "") ?cl]
                [(get-else $ ?c :changeset/group "") ?group]
                [?r :revision/changeset ?c] [?r :revision/collection :nodes]
                [?r :revision/target ?target]]
              db)
         (remove #(undone (nth % 4)))
         (keep (fn [[target at label by _ group]]
                 (when-let [id (parse-uuid (subs target (count "nodes/")))]
                   (when (exists? id) {:node/id id :label (if (= "" group) label group) :at at :by by}))))
         (sort-by :at #(compare %2 %1))
         (reduce (fn [acc {id :node/id :as e}]
                   (if (or (some #(= id (:node/id %)) acc) (>= (count acc) n)) acc (conj acc e)))
                 []))))

(defn last-edits
  "每張卡片最近一次未被復原的修改：{node-id {:at :by}}（:by 為 :user 或 :agent），一次查詢算出全部。
  建立卡片也算；沒有修改紀錄的卡片（例如只由匯入建立）不在結果中。
  規則同 recent-cards：復原本身（:undo 修改組）與匯入不算修改，已被復原的修改組不算，
  所以使用者修改後又復原，會回到先前那一筆（例如 agent 的建立）；卡片已刪除時不列。"
  [db]
  (let [exists? (set (d/q '[:find [?id ...] :where [_ :node/id ?id]] db))
        undone (undone-ids db)]
    (->> (d/q '[:find ?target ?at ?by ?cid
                :where [?c :changeset/id ?cid] [?c :changeset/at ?at] [?c :changeset/origin ?o]
                [(not= ?o :undo)] [(not= ?o :import)]
                [(get-else $ ?c :changeset/by :user) ?by]
                [?r :revision/changeset ?c] [?r :revision/collection :nodes]
                [?r :revision/target ?target]]
              db)
         (remove #(undone (nth % 3)))
         (reduce (fn [acc [target at by _]]
                   (let [id (parse-uuid (subs target (count "nodes/")))]
                     (if (and id (exists? id)
                              (or (nil? (acc id)) (pos? (compare at (:at (acc id))))))
                       (assoc acc id {:at at :by by})
                       acc)))
                 {}))))

;; ---------------------------------------------------------------------------
;; 卡片歷程（SPEC 5.4）

(defn- relation-involves?
  "關係修訂的 target（\"relations/<from>|<to>|<type>\"）是否涉及這張卡片。"
  [target node-id]
  (let [[from to] (str/split (subs target (count "relations/")) #"\|")
        id (str node-id)]
    (or (= id from) (= id to))))

(defn- node-history-input
  "某張卡片相關的修訂依修改組分好，交給 knot.history/entries。
  卡片刪除後復原是新的實體但 id 相同，所以用 target 字串查，不用 :revision/node。"
  [db node-id]
  (let [undone (undone-ids db)
        rows (concat
              (map #(into [:nodes] %)
                   (d/q '[:find ?cid ?at ?label ?by ?origin ?group ?gid ?before ?after
                          :in $ ?target
                          :where [?r :revision/target ?target] [?r :revision/changeset ?c]
                          [?c :changeset/id ?cid] [?c :changeset/at ?at] [?c :changeset/origin ?origin]
                          [(get-else $ ?c :changeset/group "") ?group]
                          [(get-else $ ?c :changeset/group-id "") ?gid]
                          [(get-else $ ?c :changeset/by :user) ?by]
                          [(get-else $ ?c :changeset/label "") ?label]
                          [(get-else $ ?r :revision/before "") ?before]
                          [(get-else $ ?r :revision/after "") ?after]]
                        db (str "nodes/" node-id)))
              (->> (d/q '[:find ?cid ?at ?label ?by ?origin ?group ?gid ?before ?after ?target
                          :where [?r :revision/collection :relations] [?r :revision/target ?target]
                          [?r :revision/changeset ?c]
                          [?c :changeset/id ?cid] [?c :changeset/at ?at] [?c :changeset/origin ?origin]
                          [(get-else $ ?c :changeset/group "") ?group]
                          [(get-else $ ?c :changeset/group-id "") ?gid]
                          [(get-else $ ?c :changeset/by :user) ?by]
                          [(get-else $ ?c :changeset/label "") ?label]
                          [(get-else $ ?r :revision/before "") ?before]
                          [(get-else $ ?r :revision/after "") ?after]]
                        db)
                   (filter #(relation-involves? (peek %) node-id))
                   (map #(into [:relations] (pop %)))))
        snapshot (fn [s] (when-not (str/blank? s) (read-edn s)))]
    (->> rows
         (group-by (comp second))
         vals
         (map (fn [rs]
                (let [[_ cid at label by origin group gid] (first rs)
                      pair (fn [[_ _ _ _ _ _ _ _ before after]] {:before (snapshot before) :after (snapshot after)})
                      node-rev (some #(when (= :nodes (first %)) (pair %)) rs)]
                  (cond-> {:changeset/id cid :at at :label label :by by :origin origin
                           :undone? (contains? undone cid)
                           :relations (into [] (comp (filter #(= :relations (first %))) (map pair)) rs)}
                    (not= "" gid) (assoc :group group :group-id gid)
                    node-rev (assoc :node node-rev))))))))

(defn node-history
  "卡片的修改歷程，舊到新（格式見 knot.history/entries）。卡片從來沒有修訂時為空向量。"
  [db node-id]
  (history/entries (node-history-input db node-id)))

(defn node-change
  "某次修改的本文前後（格式見 knot.history/prose-change）；這張卡片沒有這個修改組時為 nil。
  changeset-id 可以是合併項目中的任何一個修改組。"
  [db node-id changeset-id]
  (history/prose-change (node-history-input db node-id) changeset-id))
