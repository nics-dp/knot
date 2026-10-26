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
   :node/unlocked-status kw
   :node/version long*
   :node/label (assoc string :db/unique :db.unique/value)
   :node/facts refs
   :node/rows refs
   :node/table ref-one
   :node/table-view string
   :node/figure-file string
   :node/caption string
   :node/contract-items refs

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
   :block/ai-split bool
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
   :fact/row ref-one
   :fact/column string
   :fact/block ref-one

   ;; 引用與契約
   :citation/id (id-attr)
   :citation/node ref-one
   :citation/block ref-one
   :citation/row ref-one
   :citation/where string
   :citation/order long*
   :contract/code (assoc string :db/unique :db.unique/identity)
   :contract/text string

   ;; 修改組與修訂
   :changeset/id (id-attr)
   :changeset/origin kw
   :changeset/at {:db/valueType :db.type/instant}
   :changeset/label string
   :changeset/undoes ref-one
   :revision/changeset ref-one
   :revision/collection kw
   :revision/target string
   :revision/node ref-one
   :revision/version long*
   :revision/kind kw
   :revision/before string
   :revision/after string

   ;; 修改建議（M5）
   :proposal/trigger ref-one
   :proposal/target ref-one
   :proposal/base-version long*
   :proposal/dependency kw
   :proposal/new-prose string
   :proposal/state kw})

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
;; 連線：一個報告目錄就是一份報告，資料庫在 <報告目錄>/.knot/db

(defn db-dir [report-dir]
  (io/file report-dir ".knot" "db"))

(defn report-info [db]
  (d/q '[:find (pull ?r [:report/id :report/title :report/subtitle :report/style-name]) .
         :where [?r :report/id]]
       db))

(defn open-report
  "開啟報告目錄中的報告；還沒有報告時建立一份空白報告，名稱取自目錄名稱。回傳連線。"
  [report-dir]
  (let [dir (db-dir report-dir)
        _ (.mkdirs dir)
        c (d/get-conn (str dir) schema)]
    (when-not (report-info (d/db c))
      (d/transact! c [{:report/id (random-uuid)
                       :report/title (.getName (.getCanonicalFile (io/file report-dir)))}]))
    c))

(defn close-report [conn]
  (d/close conn))

(defn update-report!
  "修改報告的名稱等資訊（不屬於修改組）。"
  [conn info]
  (let [id (:report/id (report-info (d/db conn)))]
    (d/transact! conn [(merge (select-keys info [:report/title :report/subtitle :report/style-name])
                              {:report/id id})])))

(defn empty-report? [db]
  (empty? (d/q '[:find [?e ...] :where [?e :node/id]] db)))

;; ---------------------------------------------------------------------------
;; 讀取：資料庫 → 前端形式

(def ^:private node-scalars
  [:node/kind :node/order-key :node/name :node/title :node/draft :node/status :node/unlocked-status
   :node/label :node/figure-file :node/caption :node/table-view])

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
  (into [:node/id :node/prose :node/version
         {:node/parent [:node/id]}
         {:node/contract-items [:contract/code]}]
        node-scalars))

(defn- node->client [citations e]
  (let [id (:node/id e)
        cites (get citations id)]
    (cond-> (select-keys e (concat [:node/id :node/version] node-scalars))
      true (assoc :node/parent (get-in e [:node/parent :node/id]))
      (:node/prose e) (assoc :node/prose (read-json (:node/prose e)))
      (seq (:node/contract-items e)) (assoc :node/contract-items (vec (sort (map :contract/code (:node/contract-items e)))))
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
  [:db/id :block/id :block/kind :block/title :block/text :block/location :block/status :block/ai-split
   {:block/source [:source/file-name]}
   {:block/table [:db/id :table/kind :table/columns]}])

(defn- block->client [db e]
  (let [table (:block/table e)
        columns (some-> (:table/columns table) read-edn)]
    (cond-> (-> (select-keys e [:block/id :block/kind :block/title :block/text :block/location
                                :block/status :block/ai-split])
                (assoc :block/source (get-in e [:block/source :source/file-name])))
      (:table/kind table) (assoc :table/kind (:table/kind table))
      columns (assoc :table/columns (mapv :col/name columns)
                     :table/rows (table-rows db (:db/id table) columns)))))

(defn- read-blocks [db]
  (->> (pull-all db :block/id block-pattern)
       (mapv #(block->client db %))
       (sort-by #(str (:block/source %) " " (:block/location %) " " (:block/title %)))
       vec))

(defn- read-facts [db]
  (->> (pull-all db :fact/key [:fact/key :fact/label :fact/value :fact/unit {:fact/block [:block/id]}])
       (mapv (fn [e]
               (cond-> (dissoc e :fact/block)
                 (:fact/block e) (assoc :fact/block (get-in e [:fact/block :block/id])))))
       (sort-by :fact/key)
       vec))

(defn load-state
  "整份報告的前端形式：{:report :nodes :relations :blocks :facts}。"
  [db]
  {:report (dissoc (report-info db) :db/id)
   :nodes (read-nodes db)
   :relations (read-relations db)
   :blocks (read-blocks db)
   :facts (read-facts db)})

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
    :facts (some #(when (= k (:fact/key %)) %) (read-facts db))))

;; ---------------------------------------------------------------------------
;; 寫入：前端形式 → 交易

(def ^:private content-keys
  "這些欄位變動時，卡片的 :node/version 加一。"
  [:node/kind :node/name :node/title :node/draft :node/prose])

(defn- exists? [db attr v]
  (some? (d/entid db [attr v])))

(defn- prose-fact-keys [prose]
  (->> (tree-seq coll? seq prose)
       (keep #(when (and (map? %) (= "fact" (:type %))) (get-in % [:attrs :key])))
       distinct))

(defn- node-tx
  "卡片的交易資料。ref 把 [類型 識別鍵] 轉成這次交易可用的參照（同一交易新增的實體用 tempid）。"
  [db ref before node]
  (let [id (:node/id node)
        eid [:node/id id]
        existed? (some? before)
        version (if (= (select-keys before content-keys) (select-keys node content-keys))
                  (:node/version before 1)
                  (inc (or (:node/version before) 0)))
        removed (for [a (concat node-scalars [:node/prose :node/parent])
                      :when (and existed? (some? (get before a)) (nil? (get node a)))]
                  [:db/retract eid a])
        old-cites (when existed?
                    (d/q '[:find [?c ...] :in $ ?nid :where [?n :node/id ?nid] [?c :citation/node ?n]] db id))
        fact-keys (filter #(ref :fact %) (prose-fact-keys (:node/prose node)))]
    (concat
     ;; 清掉多值參照，下面重新加入
     (when existed?
       [[:db/retract eid :node/contract-items]
        [:db/retract eid :node/facts]])
     removed
     (for [c old-cites] [:db/retractEntity c])
     [(cond-> (merge {:db/id (ref :node id) :node/id id :node/version version}
                     (into {} (keep (fn [a] (when-some [v (get node a)] [a v]))) node-scalars))
        (:node/parent node) (assoc :node/parent (ref :node (:node/parent node)))
        (:node/prose node) (assoc :node/prose (write-json (:node/prose node)))
        (seq fact-keys) (assoc :node/facts (mapv #(ref :fact %) fact-keys)))]
     (for [code (:node/contract-items node)]
       {:db/id (str "contract-" code) :contract/code code})
     (for [code (:node/contract-items node)]
       [:db/add (ref :node id) :node/contract-items (str "contract-" code)])
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
       (for [a [:block/title :block/text :block/location :block/status :block/ai-split]
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
                    [:block/kind :block/title :block/text :block/location :block/status :block/ai-split])
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
       (for [a [:fact/label :fact/value :fact/unit :fact/block]
             :when (and (some? (get before a)) (nil? (get f a)))]
         [:db/retract eid a]))
     [(cond-> (into {:db/id (ref :fact (:fact/key f)) :fact/key (:fact/key f)}
                    (keep (fn [a] (when-some [v (get f a)] [a v])))
                    [:fact/label :fact/value :fact/unit])
        (and (:fact/block f) (ref :block (:fact/block f))) (assoc :fact/block (ref :block (:fact/block f))))])))

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
    :facts [[:db/retractEntity [:fact/key k]]]))

(def ^:private collection-order
  "交易內的順序：素材與事實先於卡片（卡片引用它們），關係最後。"
  [:blocks :facts :nodes :relations])

(def ^:private key-fns
  {:nodes :node/id :relations tree/relation-key :blocks :block/id :facts :fact/key})

(defn apply-changes!
  "把前端的變動（knot.sync/diff 的格式）寫成一個修改組。回傳 changeset id；沒有變動時為 nil。"
  [conn {:keys [origin label changes undoes]}]
  (when (seq changes)
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
                                    :facts (fact-tx ref before e)))))
                            upsert)
                    (mapcat (fn [k]
                              (when-let [before (entity-before db coll k)]
                                (revise! coll k before nil)
                                (delete-tx db coll k)))
                            delete))))
               collection-order))]
      (when (seq @revisions)
        (d/transact! conn (concat tx
                                  [(cond-> {:db/id "changeset"
                                            :changeset/id cs-id
                                            :changeset/origin (or origin :manual)
                                            :changeset/at (java.util.Date.)
                                            :changeset/label (or label "")}
                                     undoes (assoc :changeset/undoes [:changeset/id undoes]))]
                                  @revisions))
        cs-id))))

;; ---------------------------------------------------------------------------
;; 復原

(defn- undoable-changeset
  "最近一個尚未復原、也不是復原本身的修改組。"
  [db]
  (let [undone (set (d/q '[:find [?t ...] :where [_ :changeset/undoes ?u] [?u :changeset/id ?t]] db))]
    (->> (d/q '[:find ?id ?at ?label :where
                [?c :changeset/id ?id] [?c :changeset/at ?at] [?c :changeset/origin ?o]
                [(not= ?o :undo)] [(not= ?o :import)]
                [(get-else $ ?c :changeset/label "") ?label]]
              db)
         (remove (comp undone first))
         (sort-by second)
         last)))

(defn undo!
  "復原最近一個修改組：新增一個把各實體還原為修改前快照的修改組。回傳被復原的 label；沒有可復原的時為 nil。"
  [conn]
  (let [db (d/db conn)]
    (when-let [[cs-id _ label] (undoable-changeset db)]
      (let [revs (d/q '[:find ?coll ?target ?before
                        :in $ ?cid
                        :where [?c :changeset/id ?cid] [?r :revision/changeset ?c]
                        [?r :revision/collection ?coll] [?r :revision/target ?target]
                        [(get-else $ ?r :revision/before "") ?before]]
                      db cs-id)
            changes (reduce (fn [acc [coll target before]]
                              (let [k (let [s (subs target (inc (count (name coll))))]
                                        (if (#{:nodes :blocks} coll) (parse-uuid s) s))]
                                (if (str/blank? before)
                                  (update-in acc [coll :delete] (fnil conj []) k)
                                  (update-in acc [coll :upsert] (fnil conj []) (read-edn before)))))
                            {} revs)]
        (apply-changes! conn {:origin :undo :label (str "復原：" label) :changes changes :undoes cs-id})
        label))))

;; ---------------------------------------------------------------------------
;; 卡片的前一個版本

(def ^:private previous-fields
  "只保留本文的前一版；意圖不需要歷程。"
  [:node/prose])

(def ^:private session-gap-ms
  "連續的「編輯文字」修改組相隔不超過這段時間，視為同一次修改。"
  (* 10 60 1000))

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
                                     (< (- (inst-ms (:at newer)) (inst-ms (:at r))) session-gap-ms))
                              r
                              (reduced newer)))
                          latest older)]
        {:value (get-in start [:before field]) :at (:at latest) :label (:label latest)}))))

(defn node-previous
  "卡片本文在最近一次修改之前的內容：{:node/prose {:value :at :label}}；沒有修改過時為空 map。"
  [db node-id]
  (let [revs (node-revisions db node-id)]
    (into {} (keep (fn [f] (some->> (field-previous revs f) (vector f)))) previous-fields)))
