(ns knot.note
  "留言與待辦（SPEC 5.11）的純函式：留言串、待辦數、agent 的提問。
  notes 是後端 load-state 的 :notes（依時間排序的扁平清單）；
  所屬卡片已刪除的留言不顯示、不計入待辦。"
  (:require [clojure.string :as str]
            [knot.tree :as tree]))

(defn visible
  "畫面上看得到的留言：整份報告的留言，或所屬卡片仍存在的留言。"
  [nodes notes]
  (filterv #(or (nil? (:note/node %)) (contains? nodes (:note/node %))) notes))

(defn open-todo? [n]
  (and (:note/todo n) (= :open (:note/state n))))

(defn- replies-by-parent [notes]
  (group-by :note/reply-to (filter :note/reply-to notes)))

(defn- all-replies
  "某則留言的所有回覆（含回覆的回覆），依時間排序。"
  [by-parent id]
  (->> (tree-seq (constantly true) #(get by-parent (:note/id %)) {:note/id id})
       rest
       (sort-by :note/at)
       vec))

(defn threads
  "某張卡片（node-id 為 nil 時是整份報告）的留言串：最上層的留言依時間排序，
  每則帶著它的回覆 [{:note :replies [...]}]。"
  [notes node-id]
  (let [by-parent (replies-by-parent notes)]
    (->> notes
         (filter #(and (nil? (:note/reply-to %)) (= node-id (:note/node %))))
         (sort-by :note/at)
         (mapv (fn [n] {:note n :replies (all-replies by-parent (:note/id n))})))))

(defn todo-counts
  "每張卡片未完成的待辦數 {卡片 id n}；整份報告的待辦不列。"
  [nodes notes]
  (->> (visible nodes notes)
       (filter #(and (open-todo? %) (:note/node %)))
       (map :note/node)
       frequencies))

(declare agent-questions)

(defn question-counts
  "每張卡片還沒有人回覆的 agent 提問數 {卡片 id n}；整份報告的提問不列。"
  [nodes notes]
  (->> (agent-questions nodes notes)
       (keep :note/node)
       frequencies))

(defn first-questions
  "每張卡片最早一則等你回覆的提問 {卡片 id 留言 id}，供數字直接打開回覆框。"
  [nodes notes]
  (reduce (fn [m {:note/keys [id node]}]
            (cond-> m (and node (not (contains? m node))) (assoc node id)))
          {} (agent-questions nodes notes)))

(defn subtree-todo-count
  "卡片與其所有下層卡片的數量合計（counts 為 todo-counts 或 question-counts），供全文大綱的標題使用。"
  [nodes counts id]
  (reduce + (map #(get counts % 0) (cons id (tree/descendant-ids nodes id)))))

(defn open-todos
  "整份報告未完成的待辦，依時間排序。"
  [nodes notes]
  (filterv open-todo? (sort-by :note/at (visible nodes notes))))

(defn agent-questions
  "agent 的提問：agent 寫的最上層留言、還沒有任何回覆（SPEC 5.11）。"
  [nodes notes]
  (let [replied (set (keep :note/reply-to notes))]
    (filterv #(and (= :agent (:note/author %)) (nil? (:note/reply-to %))
                   (not (replied (:note/id %))))
             (sort-by :note/at (visible nodes notes)))))

(defn descendant-items
  "標題底下（後代卡片，不含自己）等你回覆的提問與未完成的待辦事項：
  [{:note 留言 :kind :question|:todo}]。提問在前、待辦事項在後，同種類依文件順序（同一張卡片依時間）。"
  [nodes notes id]
  (let [order (into {} (map-indexed (fn [i c] [c i]) (tree/descendant-ids nodes id)))
        mine? #(contains? order (:note/node %))
        sorted #(sort-by (juxt (comp order :note/node) :note/at) %)
        qs (filterv mine? (agent-questions nodes notes))
        q-ids (set (map :note/id qs))
        ts (filterv #(and (mine? %) (open-todo? %) (not (q-ids (:note/id %))))
                    (visible nodes notes))]
    (into (mapv #(hash-map :note % :kind :question) (sorted qs))
          (map #(hash-map :note % :kind :todo) (sorted ts)))))

;; ---------------------------------------------------------------------------
;; Knot 的待辦：使用者改了段落後，提醒相關段落（SPEC 5.6）

(defn card-ref
  "留言文字中的卡片稱呼：所屬標題編號加名稱，例如「2.3 指標達成總表」（不含引號）。"
  [nodes numbers id]
  (let [title (tree/display-title nodes numbers id)
        pos (tree/position-label nodes numbers id)]
    (if (and pos (not (tree/heading? nodes id))) (str pos " " title) title)))

(defn changed-paragraphs
  "兩個狀態之間意圖或本文有變動的段落（兩邊都存在的）：依文件順序的 [{:id :draft? :prose?}]。"
  [before after]
  (vec (for [id (tree/document-order (:nodes after))
             :when (tree/paragraph? (:nodes after) id)
             :let [b (get-in before [:nodes id]) a (get-in after [:nodes id])]
             :when b
             :let [draft? (not= (:node/draft b) (:node/draft a))
                   prose? (not= (:node/prose b) (:node/prose a))]
             :when (or draft? prose?)]
         {:id id :draft? draft? :prose? prose?})))

(def ^:private reason-phrases
  {"摘要它" "摘要它"
   "是它的上層引言" "是它的上層引言"
   "因果承接" "因果承接它"
   "遞進承接" "遞進承接它"
   "以它為依據" "以它為依據"})

(defn changed-writables
  "兩個狀態之間意圖或本文有變動的段落或引言（兩邊都存在的），依文件順序的 [{:id :draft? :prose?}]。
  只供「同組改寫自動完成」判斷用；誰會觸發待辦仍看 `changed-paragraphs`（引言不觸發）。"
  [before after]
  (vec (for [id (tree/document-order (:nodes after))
             :when (#{:paragraph :intro} (tree/kind (:nodes after) id))
             :let [b (get-in before [:nodes id]) a (get-in after [:nodes id])]
             :when b
             :let [draft? (not= (:node/draft b) (:node/draft a))
                   prose? (not= (:node/prose b) (:node/prose a))]
             :when (or draft? prose?)]
         {:id id :draft? draft? :prose? prose?})))

(defn- what-changed [entries]
  (let [kinds (into #{} (map #(cond (and (contains? (:fields %) :draft) (contains? (:fields %) :prose)) :both
                                    (contains? (:fields %) :prose) :prose
                                    :else :draft))
                    entries)]
    (case (when (= 1 (count kinds)) (first kinds))
      :both "意圖與本文" :prose "本文" :draft "意圖" nil)))

(defn todo-text
  "Knot 待辦的文字。triggers 為 [{:id 觸發段落 :reason 依賴原因 :fields #{:draft :prose}}]（依附加順序）；
  nodes 與 numbers 取自修改後的狀態。一個觸發：「「1.2 背景」的本文改了，本段摘要它，可能需要跟著更新。」；
  多個：「「2.3 甲」「2.4 乙」改了，本段因果承接它們、摘要它們，可能需要跟著更新。」。"
  [nodes numbers triggers]
  (let [one? (= 1 (count triggers))
        phrase (fn [r] (let [p (reason-phrases r)] (if one? p (str/replace p "它" "它們"))))]
    (str (str/join (for [t triggers] (str "「" (card-ref nodes numbers (:id t)) "」")))
         (if one? (str "的" (what-changed triggers) "改了") "改了")
         "，本段" (str/join "、" (distinct (map (comp phrase :reason) triggers)))
         "，可能需要跟著更新。")))

(defn todo-lead
  "畫面上 Knot 待辦的說明（段落名稱另以可點的標籤列在後面，所以不重複寫出）：
  一個觸發：「這段的本文改了，本段摘要它，可能需要跟著更新：」；
  多個：「這些段落改了，本段因果承接它們、摘要它們，可能需要跟著更新：」。"
  [triggers]
  (let [one? (= 1 (count triggers))
        phrases (distinct (keep #(some-> (reason-phrases (:reason %))
                                         (cond-> (not one?) (str/replace "它" "它們")))
                                triggers))
        ;; 舊資料的觸發可能沒有記下欄位與原因
        what (when (every? (comp seq :fields) triggers) (what-changed triggers))]
    (str (cond (not one?) "這些段落改了" what (str "這段的" what "改了") :else "這段改了")
         (when (seq phrases) (str "，本段" (str/join "、" phrases)))
         "，可能需要跟著更新：")))

(defn knot-todos
  "修改（使用者或 agent，before → after）要留下的 Knot 待辦（SPEC 5.6）：
  [{:op :add :note/node 目標 :note/triggers [...] :note/text 文字 :reason 新增的原因}
   {:op :update :note/id 現有待辦 :note/node 目標 :note/triggers [...全部] :note/text 文字 :reason ...}]。
  每個觸發 {:id 觸發的段落 :dependency :summary|:argument :reason 原因 :fields #{:draft :prose}}。
  每個目標段落最多一則未完成的 Knot 待辦：已有時不新增，把新的觸發卡片附加上去（:update，文字重寫），
  已經記著這個觸發卡片時文字不動（連續輸入只留一則），但該觸發的 :group-id 更新為這次的
  （沒有 group 時移除）：待辦對應觸發卡片最新的修改；只有 group-id 變了的 :update 帶 :refresh-only? true。目標還沒有本文（尚未撰寫）時不留。
  依賴取自修改後的關係（tree/dependents）；notes 是現有的留言。標題、引言、表、圖的修改不觸發。"
  ([before after notes] (knot-todos before after notes nil))
  ([before after notes {:keys [group-id]}]
  (let [nodes (:nodes after)
        numbers (tree/numbering nodes)
        open (into {} (keep #(when (and (= :knot (:note/author %)) (open-todo? %)) [(:note/node %) %])) notes)
        order (zipmap (tree/document-order nodes) (range))
        changed (changed-paragraphs before after)
        ;; 同一組內、這次一起改寫的目標不必再留待辦（它本來就依新的前文改寫）
        rewritten (when group-id (set (map :id (changed-writables before after))))
        entries (for [{trigger :id :keys [draft? prose?]} changed
                      {:keys [id dependency reason]} (tree/dependents nodes (:relations after) trigger)
                      :when (seq (:content (get-in nodes [id :node/prose])))
                      :when (not (contains? rewritten id))]
                  [id (cond-> {:id trigger :dependency dependency :reason reason
                               :fields (cond-> #{} draft? (conj :draft) prose? (conj :prose))}
                        group-id (assoc :group-id group-id))])
        by-target (group-by first entries)]
    (vec (for [target (sort-by order (keys by-target))
               :let [existing (get open target)
                     have (set (map :id (:note/triggers existing)))
                     incoming (map second (by-target target))
                     new (vec (remove #(have (:id %)) incoming))
                     latest (into {} (map (juxt :id identity)) incoming)
                     refreshed (mapv (fn [t] (if (latest (:id t))
                                               (if group-id (assoc t :group-id group-id) (dissoc t :group-id))
                                               t))
                                     (:note/triggers existing))
                     refresh? (and existing (not= refreshed (vec (:note/triggers existing))))]
               :when (or (seq new) refresh?)
               :let [all (into refreshed new)
                     note {:note/node target :note/triggers all
                           :note/text (if (seq new) (todo-text nodes numbers all) (:note/text existing))
                           :reason (str/join "、" (distinct (map :reason new)))}]]
           (if existing
             (cond-> (assoc note :op :update :note/id (:note/id existing))
               (empty? new) (assoc :refresh-only? true))
             (assoc note :op :add)))))))

(defn auto-complete-ids
  "同一個 group 內改寫後自動完成的 Knot 待辦（SPEC 5.6 第 2 點）：這次寫入（group-id 為字串，
  沒有 group 時為 nil，回傳空）改了意圖或本文的段落，若有未完成的 Knot 待辦，且該待辦
  所有觸發的 :group-id 都等於 group-id，就是要完成的。回傳 [{:note/id :note/node}]（依文件順序）。
  使用者的寫入與沒有 group 的寫入不會完成任何待辦。"
  [before after notes group-id]
  (if (nil? group-id)
    []
    (let [changed (set (map :id (changed-writables before after)))
          order (zipmap (tree/document-order (:nodes after)) (range))]
      (->> notes
           (filter #(and (= :knot (:note/author %)) (open-todo? %)
                         (contains? changed (:note/node %))
                         (seq (:note/triggers %))
                         (every? (fn [t] (= group-id (:group-id t))) (:note/triggers %))))
           (sort-by (comp order :note/node))
           (mapv #(select-keys % [:note/id :note/node]))))))
