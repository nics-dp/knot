(ns knot.history
  "卡片歷程：把某張卡片相關的修訂（依修改組分好）整理成歷程項目。純函式，不碰資料庫。

  輸入（每個修改組一筆，順序不拘）：
  {:changeset/id uuid :at inst :label str :by kw :origin kw :undone? bool
   :group 組名 :group-id uuid                           ; 選填：agent 一次工作的多次寫入（B1）
   :node {:before 卡片快照或 nil :after 卡片快照或 nil}   ; 這張卡片在這個修改組的修訂（可能沒有）
   :relations [{:before 關係或 nil :after 關係或 nil}]}  ; 涉及這張卡片的關係修訂")

(def session-gap-ms
  "連續的「編輯文字」修改組相隔不超過這段時間，視為同一次修改。"
  (* 10 60 1000))

(def ^:private typing-label "編輯文字")

(def ^:private ignored-keys
  "不算「改了什麼」的欄位：識別與版本號。"
  #{:node/id :node/version})

(defn- field-of
  "快照欄位歸入哪一類歷程欄位；其他欄位都算 :settings（label、圖檔、caption、契約項目等）。"
  [k]
  (case k
    (:node/name :node/title) :name
    :node/draft :intent
    :node/prose :prose
    :node/table-content :table
    :node/citations :basis
    (:node/parent :node/order-key) :position
    :node/status :status
    :settings))

(defn- name-of [node] (or (:node/name node) (:node/title node)))

(defn- changed-fields
  "兩個快照之間有變的歷程欄位集合；任一邊為 nil（建立、刪除）時為空集合。"
  [before after]
  (if (and before after)
    (into #{}
          (comp (remove ignored-keys)
                (filter #(not= (get before %) (get after %)))
                (map field-of))
          (set (concat (keys before) (keys after))))
    #{}))

(defn- label-of
  "歷程項目的名稱：屬於某個 group 時用組名，否則是修改組的 label。"
  [g]
  (or (:group g) (:label g)))

(defn- mergeable?
  "兩個相鄰的修改組是否合併成同一次修改：
  同一個 group-id；或都是同一人的「編輯文字」、都修改了這張卡片、相隔不超過 session-gap-ms。"
  [a b]
  (let [edit? #(and (= typing-label (:label %))
                    (get-in % [:node :before]) (get-in % [:node :after]))]
    (or (and (:group-id a) (= (:group-id a) (:group-id b)))
        (and (edit? a) (edit? b)
             (= (:by a) (:by b))
             (<= (- (inst-ms (:at b)) (inst-ms (:at a))) session-gap-ms)))))

(defn- merge-group
  "把一串連續的修改組（舊到新）合成一筆：快照取最早的 before 與最晚的 after。"
  [revs]
  (let [with-node (filter :node revs)]
    (cond-> (assoc (last revs)
                   :changeset-ids (mapv :changeset/id revs)
                   :merged (count revs)
                   :undone? (boolean (some :undone? revs))
                   :relations (vec (mapcat :relations revs)))
      (seq with-node) (assoc :node {:before (get-in (first with-node) [:node :before])
                                    :after (get-in (last with-node) [:node :after])}
                             ;; 每個修改組各自改了哪些欄位的聯集（改了又改回去的欄位也算）
                             :member-fields (when (:group-id (last revs))
                              (into #{} (mapcat #(changed-fields (get-in % [:node :before])
                                                                               (get-in % [:node :after])))
                                                  with-node))))))

(defn- groups
  "依時間排序後，把可以合併的相鄰修改組合併：[{:changeset-ids :merged :node :relations ...}]，舊到新。"
  [revs]
  (->> (sort-by #(inst-ms (:at %)) revs)
       (reduce (fn [acc r]
                 (let [prev (peek acc)]
                   (if (and prev (mergeable? (last prev) r))
                     (conj (pop acc) (conj prev r))
                     (conj acc [r]))))
               [])
       (mapv merge-group)))

(defn- kind-of [{:keys [node origin]}]
  (let [{:keys [before after]} node]
    (cond
      (= :undo origin) :undo
      (nil? node) :change
      (= :import origin) :import
      (nil? after) :delete
      (nil? before) :create
      :else :change)))

(defn- entry [{:keys [node relations] :as g}]
  (let [{:keys [before after]} node
        kind (kind-of g)
        ;; 建立、匯入、刪除不列欄位（連帶的關係變動也不列）
        fields (if (#{:create :import :delete} kind)
                 #{}
                 (cond-> (into (changed-fields before after) (:member-fields g))
                   (seq relations) (conj :relations)))
        text (fn [field f]
               (let [b (f before) a (f after)]
                 (when (and (contains? fields field) (not= b a))
                   {field {:before b :after a}})))]
    (when (or (not= :change kind) (seq fields))
      {:changeset-ids (:changeset-ids g)
       :at (:at g)
       :label (label-of g)
       :by (or (:by g) :user)
       :origin (:origin g)
       :kind kind
       :undone? (boolean (:undone? g))
       :fields fields
       :text-changes (merge (text :name name-of) (text :intent :node/draft))
       :merged (:merged g)})))

(defn entries
  "歷程項目，舊到新。格式：
  {:changeset-ids [uuid ...]  ; 合併的修改組，舊到新；最後一個是代表 id
   :at inst :label str :by kw :origin kw
   :kind kw                   ; :create :import :delete :undo :change
   :undone? bool              ; 合併的項目任一個被復原就算
   :fields #{:name :intent :prose :table :basis :relations :position :status :settings}
   :text-changes {:name {:before :after} :intent {:before :after}}  ; 只有名稱、意圖有變時
   :merged n}                 ; 合併了幾個修改組
  連續的逐字輸入（同一人、「編輯文字」、相隔 10 分鐘內）合併成一筆；在這張卡片的歷程上相鄰、
  group-id 相同的修改組也合併成一筆（label 為組名，:fields 取各修改組的聯集，文字取最早 before 與最晚 after）；建立、匯入、刪除的 :fields 為空；
  沒有任何欄位變動的修改組（例如只改了版本號）不列。"
  [revs]
  (into [] (keep entry) (groups revs)))

(defn prose-change
  "包含 changeset-id 的項目的本文前後：{:prose? true :before :after :at :label :by}；合併的項目取最早的 before、最晚的 after。
  那筆沒有改本文時為 {:prose? false :at :label :by}（例如待辦連到的修改只改了意圖）；找不到時為 nil。"
  [revs changeset-id]
  (when-let [g (some #(when (some #{changeset-id} (:changeset-ids %)) %) (groups revs))]
    (let [meta {:at (:at g) :label (label-of g) :by (or (:by g) :user)}]
      (if (contains? (:fields (entry g)) :prose)
        (assoc meta :prose? true
               :before (get-in g [:node :before :node/prose])
               :after (get-in g [:node :after :node/prose]))
        (assoc meta :prose? false)))))

(defn- at-ms
  "時間可以是 inst 或毫秒數。"
  [t]
  (if (number? t) t (inst-ms t)))

(defn timeline
  "留言與歷程穿插成一條時間軸，舊到新：[{:type :thread :at :thread} {:type :change :at :entry}]。
  threads 是 knot.note/threads 的結果（位置依最上層留言的時間）；同一時間時修改在前。"
  [threads entries]
  (->> (concat (for [e entries] {:type :change :at (:at e) :entry e})
               (for [t threads] {:type :thread :at (get-in t [:note :note/at]) :thread t}))
       (sort-by (juxt #(at-ms (:at %)) #(if (= :change (:type %)) 0 1)))
       vec))
