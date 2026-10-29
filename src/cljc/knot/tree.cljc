(ns knot.tree
  "卡片樹的純函式：查詢、編號、合法性判斷與移動。
  nodes 是 {id node} 的 map，node 以 :node/parent 與 :node/order-key 構成樹。"
  (:require [knot.order-key :as ok]))

(def max-heading-level 4)

;; ---------------------------------------------------------------------------
;; 查詢

(defn children
  "parent-id 的子卡片，依 order-key 排序；parent-id 為 nil 時回傳第一層標題。"
  [nodes parent-id]
  (->> (vals nodes)
       (filter #(= parent-id (:node/parent %)))
       (sort-by :node/order-key)
       vec))

(defn child-ids [nodes parent-id]
  (mapv :node/id (children nodes parent-id)))

(defn parent [nodes id]
  (get-in nodes [id :node/parent]))

(defn ancestor-ids
  "由近到遠的祖先 id。"
  [nodes id]
  (vec (take-while some? (rest (iterate #(parent nodes %) id)))))

(defn depth
  "第一層標題的深度為 1。"
  [nodes id]
  (inc (count (ancestor-ids nodes id))))

(defn descendant-ids [nodes id]
  (vec (mapcat (fn [c] (cons c (descendant-ids nodes c)))
               (child-ids nodes id))))

(defn descendant? [nodes ancestor-id id]
  (boolean (some #{ancestor-id} (ancestor-ids nodes id))))

(defn document-order
  "整份報告依閱讀順序（深度優先）排列的 id。"
  ([nodes] (document-order nodes nil))
  ([nodes parent-id]
   (vec (mapcat (fn [c] (cons c (document-order nodes c)))
                (child-ids nodes parent-id)))))

(defn kind [nodes id]
  (get-in nodes [id :node/kind]))

(defn heading? [nodes id] (= :heading (kind nodes id)))
(defn paragraph? [nodes id] (= :paragraph (kind nodes id)))
(defn intro? [nodes id] (= :intro (kind nodes id)))

(defn heading-level
  "標題的層級等於它在樹中的深度。"
  [nodes id]
  (when (heading? nodes id) (depth nodes id)))

(defn subtree-heading-height
  "以 id 為根的子樹中，標題往下延伸的層數（只有自己時為 1）。"
  [nodes id]
  (if (heading? nodes id)
    (inc (reduce max 0 (map #(subtree-heading-height nodes %) (child-ids nodes id))))
    0))

(defn intro-id [nodes heading-id]
  (some #(when (= :intro (kind nodes %)) %) (child-ids nodes heading-id)))

(defn siblings [nodes id]
  (child-ids nodes (parent nodes id)))

(defn prev-sibling [nodes id]
  (let [sibs (siblings nodes id)
        i (.indexOf sibs id)]
    (when (pos? i) (nth sibs (dec i)))))

(defn next-sibling [nodes id]
  (let [sibs (siblings nodes id)
        i (.indexOf sibs id)]
    (when (< -1 i (dec (count sibs))) (nth sibs (inc i)))))

(defn prev-paragraph
  "同一 parent 下緊鄰在前的段落；id 不是段落、前面是引言、標題或沒有卡片時為 nil
  （段落一律排在下層標題之前，所以不會跨到下層標題）。"
  [nodes id]
  (when (paragraph? nodes id)
    (let [p (prev-sibling nodes id)]
      (when (and p (paragraph? nodes p)) p))))

;; ---------------------------------------------------------------------------
;; 編號

(def ^:private cn-digits ["零" "一" "二" "三" "四" "五" "六" "七" "八" "九"])

(defn chinese-numeral
  "1–99 的中文數字，例如 3 → 三、12 → 十二、20 → 二十。"
  [n]
  (let [tens (quot n 10) ones (mod n 10)]
    (cond
      (< n 10) (cn-digits n)
      :else (str (when (> tens 1) (cn-digits tens))
                 "十"
                 (when (pos? ones) (cn-digits ones))))))

(defn numbering
  "標題 id → 編號字串，例如 \"3\"、\"3.1\"、\"3.1.1.1\"。編號不儲存，一律由樹的位置計算。"
  [nodes]
  (letfn [(walk [acc parent-id prefix]
            (let [heads (filterv #(heading? nodes %) (child-ids nodes parent-id))]
              (reduce (fn [acc [i id]]
                        (let [num (str prefix (when prefix ".") (inc i))]
                          (walk (assoc acc id num) id num)))
                      acc
                      (map-indexed vector heads))))]
    (walk {} nil nil)))

(defn display-title
  "卡片上顯示的標題：章為「第三章 名稱」，其餘標題為「3.1 名稱」。"
  [nodes numbers id]
  (let [n (get nodes id)]
    (case (:node/kind n)
      :heading (let [num (get numbers id)]
                 (if (= 1 (depth nodes id))
                   (str "第" (chinese-numeral (parse-long num)) "章 " (:node/name n))
                   (str num " " (:node/name n))))
      :intro "引言"
      (:node/title n))))

(defn position-label
  "卡片在報告中的位置：所屬最近一層標題的編號，章為「第三章」，其餘為「3.1.2」；
  不在任何標題底下時為 nil。"
  [nodes numbers id]
  (when-let [h (some #(when (heading? nodes %) %) (ancestor-ids nodes id))]
    (let [num (get numbers h)]
      (if (= 1 (depth nodes h))
        (str "第" (chinese-numeral (parse-long num)) "章")
        num))))

;; ---------------------------------------------------------------------------
;; 合法性

(defn- others [nodes parent-id moving-id]
  (remove #{moving-id} (child-ids nodes parent-id)))

(defn accepts?
  "parent-id 底下能否放入 moving-id 這張卡片（moving-id 已在別處時，不把它算進現有子卡片）。
  規則見 SPEC 5.3：引言最多一張；標題底下可以同時有段落與下層標題（順序另由
  `order-ok?` 判斷：引言、段落、下層標題）；表格與圖只能掛在段落底下；
  標題連同子樹不得超過四層。"
  [nodes parent-id moving-id]
  (let [k (kind nodes moving-id)
        sibs (others nodes parent-id moving-id)
        sib-kinds (set (map #(kind nodes %) sibs))]
    (cond
      (= parent-id moving-id) false
      (and parent-id (descendant? nodes moving-id parent-id)) false

      (nil? parent-id)
      (= k :heading)

      (heading? nodes parent-id)
      (case k
        :intro (not (sib-kinds :intro))
        :paragraph true
        :heading (<= (+ (depth nodes parent-id)
                        (subtree-heading-height nodes moving-id))
                     max-heading-level)
        false)

      (paragraph? nodes parent-id)
      (contains? #{:table :figure} k)

      :else false)))

;; ---------------------------------------------------------------------------
;; 放置位置

(defn- slot-key
  "在 parent-id 的子卡片（不含 moving-id）中，插入到 index 位置時的 order-key。"
  [nodes parent-id moving-id index]
  (let [sibs (vec (others nodes parent-id moving-id))
        before (when (pos? index) (get-in nodes [(nth sibs (dec index)) :node/order-key]))
        after (when (< index (count sibs)) (get-in nodes [(nth sibs index) :node/order-key]))]
    (ok/between before after)))

(defn- kind-rank
  "同一 parent 底下的排列順序：引言、段落（含表與圖）、下層標題。"
  [k]
  (case k :intro 0 :heading 2 1))

(defn order-ok?
  "moving-id 插入 parent-id 的 index 位置（不含 moving-id 的兄弟中計算）後，
  兄弟的種類順序是否仍是：引言、段落、下層標題（SPEC 5.3）。"
  [nodes parent-id moving-id index]
  (let [sibs (vec (others nodes parent-id moving-id))
        ranks (mapv #(kind-rank (kind nodes %))
                    (concat (subvec sibs 0 index)
                            [moving-id]
                            (subvec sibs index)))]
    (apply <= ranks)))

(defn- into-index
  "移入時放在同類卡片的最後：引言放最前面，段落放在最後一個段落之後（下層標題之前），
  標題放在最後。"
  [nodes parent-id moving-id]
  (let [sibs (vec (others nodes parent-id moving-id))
        r (kind-rank (kind nodes moving-id))]
    (if (= :intro (kind nodes moving-id))
      0
      (count (take-while #(<= (kind-rank (kind nodes %)) r) sibs)))))

(defn target-slot
  "把 moving-id 放到 target-id 的 pos（:before、:after、:into）時的位置 {:parent :index}，
  不判斷合法性（合法性見 `placement`）；供說明違規原因用。"
  [nodes moving-id target-id pos]
  (if (= pos :into)
    {:parent target-id :index (into-index nodes target-id moving-id)}
    (let [p (parent nodes target-id)
          sibs (vec (others nodes p moving-id))
          i (.indexOf sibs target-id)]
      {:parent p :index (if (= pos :before) i (inc i))})))

(defn placement
  "把 moving-id 放到 target-id 的 pos（:before、:after、:into）時的新位置
  {:parent :index}；不合法時回傳 nil。"
  [nodes moving-id target-id pos]
  (when (and (contains? nodes moving-id)
             (contains? nodes target-id)
             (not= moving-id target-id)
             (not (descendant? nodes moving-id target-id)))
    (let [{p :parent index :index :as slot} (target-slot nodes moving-id target-id pos)]
      (when (and (accepts? nodes p moving-id)
                 (or (= pos :into) (order-ok? nodes p moving-id index)))
        slot))))

(defn move
  "依 placement 移動卡片：只改被移動卡片的 parent 與 order-key。"
  [nodes moving-id {:keys [parent index]}]
  (update nodes moving-id assoc
          :node/parent parent
          :node/order-key (slot-key nodes parent moving-id index)))

(defn sibling-placement
  "與相鄰的兄弟交換位置（上移 delta=-1、下移 delta=1）的 placement；不能移動時為 nil。"
  [nodes id delta]
  (let [p (parent nodes id)
        sibs (siblings nodes id)
        i (.indexOf sibs id)
        index (+ i delta)]
    (when (and (<= 0 index (dec (count sibs)))
               (order-ok? nodes p id index))
      {:parent p :index index})))

(defn insert-index
  "在 parent-id 下新增 kind 卡片時的位置：引言最前面；段落（與表、圖）接在最後一個
  段落之後、第一個下層標題之前；標題放最後。"
  [nodes parent-id kind]
  (let [sibs (children nodes parent-id)
        r (kind-rank kind)]
    (if (= kind :intro)
      0
      (count (take-while #(<= (kind-rank (:node/kind %)) r) sibs)))))

(defn insert-key
  "在 parent-id 下新增 kind 卡片時的 order-key（位置見 `insert-index`）。"
  [nodes parent-id kind]
  (let [sibs (children nodes parent-id)
        i (insert-index nodes parent-id kind)]
    (ok/between (when (pos? i) (:node/order-key (nth sibs (dec i))))
                (when (< i (count sibs)) (:node/order-key (nth sibs i))))))

(defn key-after
  "緊接在 id 後面的 order-key。"
  [nodes id]
  (ok/between (get-in nodes [id :node/order-key])
              (some->> (next-sibling nodes id) (get nodes) :node/order-key)))

;; ---------------------------------------------------------------------------
;; 段落關係

(def adjacent-relation-types
  "相鄰段落（同一 parent、前後相接）之間的關係。"
  [:progression :causal :contrast :example])

(def remote-relation-types
  "不相鄰（任意距離）的關係；from 是本段，to 是目標段落。
  摘要：本段彙整目標的結果；依據：本段論點建立在目標的結果上（論證依賴）；
  呼應、對比：不構成依賴。"
  [:summarizes :basis :echo :contrast])

(defn remote-relation?
  "不相鄰的關係以 :relation/remote 標記；摘要一律視為不相鄰。"
  [r]
  (boolean (or (:relation/remote r) (= :summarizes (:relation/type r)))))

(defn relation-key
  "關係的識別鍵：相鄰關係每對段落只有一條；不相鄰關係每對段落每種類型一條。"
  [r]
  (str (:relation/from r) "|" (:relation/to r) "|"
       (if (remote-relation? r) (name (:relation/type r)) "adjacent")))

(defn- adjacent-relation? [r]
  (not (remote-relation? r)))

(defn remote-relations
  "與 id 有關的不相鄰關係：{:outgoing [...] :incoming [...]}。"
  [relations id]
  (let [remote (filter remote-relation? relations)]
    {:outgoing (filterv #(= id (:relation/from %)) remote)
     :incoming (filterv #(= id (:relation/to %)) remote)}))

(defn relation-between [relations from to]
  (some #(when (and (= from (:relation/from %)) (= to (:relation/to %))
                    (adjacent-relation? %))
           %)
        relations))

(defn mark-pending
  "移動後重新標記段落與前一段的關係（SPEC 5.3）：affected 中每個段落，
  先移除它原本與 old-prev 的相鄰關係，再把與新的前一段之間的關係標為待確認；
  原本已有關係時保留類型。"
  [relations nodes affected old-prevs]
  (reduce
   (fn [rels id]
     (if-not (paragraph? nodes id)
       rels
       (let [old-prev (get old-prevs id)
             new-prev (prev-paragraph nodes id)
             existing (when new-prev (relation-between rels new-prev id))
             rels (->> rels
                       (remove #(and (= id (:relation/to %))
                                     (adjacent-relation? %)
                                     (#{old-prev new-prev} (:relation/from %))))
                       vec)]
         (cond-> rels
           new-prev (conj {:relation/from new-prev
                           :relation/to id
                           :relation/type (:relation/type existing)
                           :relation/pending true})))))
   relations
   (distinct (remove nil? affected))))

(defn move-with-relations
  "移動卡片並更新段落關係。回傳 {:nodes :relations}。
  受影響的段落：舊位置的下一段、被移動的段落、新位置的下一段。"
  [{:keys [nodes relations]} moving-id place]
  (let [old-next (next-sibling nodes moving-id)
        old-prevs {moving-id (prev-paragraph nodes moving-id)
                   old-next moving-id}
        nodes' (move nodes moving-id place)
        new-next (next-sibling nodes' moving-id)
        old-prevs (assoc old-prevs new-next (prev-paragraph nodes new-next))
        para? (paragraph? nodes moving-id)]
    {:nodes nodes'
     :relations (if para?
                  (mark-pending relations nodes' [old-next moving-id new-next] old-prevs)
                  relations)}))

;; ---------------------------------------------------------------------------
;; 建立

(defn from-nested
  "把巢狀的 [{:id :kind ... :children [...]}] 轉成 nodes map，依陣列順序產生 order-key。"
  [tree]
  (letfn [(walk [acc parent-id items]
            (reduce (fn [acc [item k]]
                      (let [node (-> item
                                     (dissoc :children)
                                     (assoc :node/parent parent-id :node/order-key k))]
                        (walk (assoc acc (:node/id node) node)
                              (:node/id node)
                              (:children item))))
                    acc
                    (map vector items (ok/initial-keys (count items)))))]
    (walk {} nil tree)))
