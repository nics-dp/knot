(ns knot.ops
  "agent 的卡片寫入操作（SPEC 5.12）：與畫面共用同一套規則的純函式。
  每個函式收狀態（與前端相同的形式，:nodes 為 id → 卡片）與參數，回傳
  {:state 新狀態 :created [新卡片 id] :touched [新增或修改的卡片 id]
   :origin :manual|:move :label 修改組的預設名稱}（可有 :message、:note），
  不合法時回傳 {:error \"給 agent 看的中文原因\"}，狀態一律不變。
  合法性由 knot.tree 的 accepts?／placement 判斷，這裡只負責說明原因；
  關係的處理（插入段落、移動後標為待確認）與 knot.ui 的畫面操作一致。"
  (:require [clojure.string :as str]
            [knot.tree :as tree]
            [knot.ui :as ui]))

;; ---------------------------------------------------------------------------
;; 顯示用

(defn heading-number
  "標題編號：章為「第一章」，其餘為「1.2」；非標題為 nil。"
  [nodes numbers id]
  (when (tree/heading? nodes id)
    (let [num (get numbers id)]
      (if (= 1 (tree/depth nodes id))
        (str "第" (tree/chinese-numeral (parse-long num)) "章")
        num))))

(defn- card-ref
  "錯誤說明中的卡片稱呼，例如「1.1 計畫緣起」。"
  [nodes id]
  (let [nodes (dissoc nodes ::probe)] ; 試探用的卡片不能參與編號
    (str "「" (tree/display-title nodes (tree/numbering nodes) id) "」")))

(def ^:private kind-names {:heading "標題" :paragraph "段落" :intro "引言" :table "表格" :figure "圖"})

(defn- blank? [s] (or (not (string? s)) (str/blank? s)))

;; ---------------------------------------------------------------------------
;; 合法性說明

(defn- reject-reason
  "把 moving-id（已在 nodes 中，新卡片為試探用的 probe）放進 parent-id 底下為什麼不行；
  合法時為 nil。判斷本身以 tree/accepts? 為準，這裡只是把原因說清楚。"
  [nodes parent-id moving-id]
  (let [k (tree/kind nodes moving-id)
        new? (= ::probe moving-id)
        sib-kinds (set (map #(tree/kind nodes %) (remove #{moving-id} (tree/child-ids nodes parent-id))))
        reason
        (cond
          (nil? parent-id) (when-not (= k :heading) "最上層只能放標題（章）")
          (= parent-id moving-id) "不能移到自己底下"
          (tree/descendant? nodes moving-id parent-id) "不能移到自己的下層"

          (tree/heading? nodes parent-id)
          (let [r (card-ref nodes parent-id)]
            (case k
              :intro (when (sib-kinds :intro) (str r "已經有引言，每個標題最多一張"))
              :paragraph (when (sib-kinds :heading) (str r "底下已有下層標題，不能再放段落"))
              :heading (cond
                         (sib-kinds :paragraph) (str r "底下已有段落，不能再放下層標題")
                         (> (+ (tree/depth nodes parent-id) (tree/subtree-heading-height nodes moving-id))
                            tree/max-heading-level)
                         (if (and (= 1 (tree/subtree-heading-height nodes moving-id))
                                  (= tree/max-heading-level (tree/depth nodes parent-id)))
                           (str r "已經是第四層，不能再" (if new? "新增" "放") "下層標題")
                           (str "標題連同下層共 " (tree/subtree-heading-height nodes moving-id)
                                " 層，放到" r "底下會超過四層")))
              (str r "底下不能放" (kind-names k))))

          (tree/paragraph? nodes parent-id)
          (when-not (#{:table :figure} k) (str (card-ref nodes parent-id) "是段落，底下只能放表格與圖"))

          :else (str (card-ref nodes parent-id) "底下不能放卡片"))]
    (or reason
        (when-not (tree/accepts? nodes parent-id moving-id) "不符合層級規則"))))

;; ---------------------------------------------------------------------------
;; 新增

(defn- new-node [kind title intent]
  (let [draft (or intent "")]
    (case kind
      :intro {:node/kind :intro :node/draft draft}
      :paragraph {:node/kind :paragraph :node/title title :node/status :none :node/draft draft}
      :heading {:node/kind :heading :node/name title :node/draft draft})))

(defn- field-error [kind title]
  (cond
    (not (#{:heading :paragraph :intro} kind)) "kind 只接受 heading、paragraph、intro"
    (and (= kind :intro) (not (str/blank? (when (string? title) title)))) "引言沒有名稱，不需要 title"
    (and (not= kind :intro) (blank? title)) "需要 title（標題名稱或段落標題）"))

(defn- insert-card
  "在 parent-id 底下新增一張卡片，接在 after-id 之後（省略時引言放最前面、其他放最後）。
  回傳 {:state :id} 或 {:error}。"
  [state {:keys [parent-id after-id kind title intent]}]
  (let [{:keys [nodes]} state
        probe (assoc nodes ::probe {:node/id ::probe :node/kind kind})]
    (or (some->> (field-error kind title) (hash-map :error))
        (cond
          (and (some? parent-id) (not (contains? nodes parent-id)))
          {:error (str "找不到 parent_id：" parent-id)}

          (and (some? after-id) (not (contains? nodes after-id)))
          {:error (str "找不到 after_id：" after-id)}

          (and (some? after-id) (not= parent-id (tree/parent nodes after-id)))
          {:error (str "after_id 不是 parent_id 的子卡片"
                       (when parent-id (str "（" (card-ref nodes parent-id) "的子卡片請用 get_card 查看）")))}

          (and (some? after-id) (= kind :intro))
          {:error "引言固定在最前面，新增引言時不能指定 after_id"}

          :else
          (if-let [why (reject-reason probe parent-id ::probe)]
            {:error why}
            (let [id (random-uuid)
                  k (if after-id (tree/key-after nodes after-id) (tree/insert-key nodes parent-id kind))
                  nodes' (assoc nodes id (assoc (new-node kind title intent)
                                                :node/id id :node/parent parent-id :node/order-key k))
                  next-para (tree/next-sibling nodes' id)]
              {:id id
               :state (-> state
                          (assoc :nodes nodes')
                          ;; 原本接在後面的段落改為承接新段落，關係待確認（同畫面的 add-sibling）
                          (update :relations #(if (= kind :paragraph)
                                                (tree/mark-pending % nodes' [next-para]
                                                                   {next-para (tree/prev-sibling nodes' id)})
                                                %)))}))))))

(defn add-card
  "新增一張卡片。kind 為 :heading、:paragraph、:intro；parent-id 為 nil 時新增一章。"
  [state {:keys [kind title] :as args}]
  (let [{:keys [error id] :as r} (insert-card state args)]
    (if error
      {:error error}
      {:state (:state r)
       :created [id]
       :touched [id]
       :origin :manual
       :label (str "新增" (kind-names kind) (when-not (= kind :intro) (str "「" title "」")))})))

(defn- build-items
  "依序建立 items（巢狀）；第一項接在 after-id 之後，其餘接在前一項之後。
  回傳 {:state :created} 或 {:error}（錯誤開頭是項目的位置，例如 items[1].children[0]）。"
  [state parent-id after-id items path]
  (loop [state state, prev after-id, created [], todo (seq (map-indexed vector items))]
    (if (empty? todo)
      {:state state :created created}
      (let [[i item] (first todo)
            more (rest todo)
            here (str path "[" i "]")
            fail #(hash-map :error (str here "：" %))
            {:keys [kind children]} item]
        (cond
          (not (map? item)) (fail "每一項必須是物件")
          (and (seq children) (not= kind :heading)) (fail (str (kind-names kind "這種卡片") "不能有 children，只有標題能有下層卡片"))
          (and children (not (sequential? children))) (fail "children 必須是陣列")
          :else
          (let [r (insert-card state (assoc item :parent-id parent-id
                                            :after-id (when-not (= kind :intro) prev)))]
            (if (:error r)
              (fail (:error r))
              (let [id (:id r)
                    sub (when (seq children)
                          (build-items (:state r) id nil children (str here ".children")))]
                (if (:error sub)
                  sub
                  (recur (or (:state sub) (:state r))
                         (if (= kind :intro) prev id)
                         (into (conj created id) (:created sub))
                         more))))))))))

(defn build-outline
  "一次建立巢狀骨架：items 為 [{:kind :title :intent :children [...]}]。
  任何一項不合法就整批不寫。created 依文件順序列出所有新卡片。"
  [state {:keys [parent-id after-id items]}]
  (cond
    (not (sequential? items)) {:error "items 必須是陣列"}
    (empty? items) {:error "items 不可為空"}
    :else
    (let [r (build-items state parent-id after-id items "items")]
      (if (:error r)
        r
        (let [made (set (:created r))
              created (filterv made (tree/document-order (:nodes (:state r))))]
          {:state (:state r)
           :created created
           :touched created
           :origin :manual
           :label (str "建立大綱：" (count created) " 張卡片")})))))

;; ---------------------------------------------------------------------------
;; 修改

(defn update-card
  "修改卡片的名稱（title）或意圖（intent）；不改本文。
  給了 version 且與目前版本不同時不寫入（SPEC 5.13）。"
  [state {:keys [id title intent version]}]
  (let [{:keys [nodes]} state
        node (get nodes id)
        k (:node/kind node)]
    (cond
      (nil? node) {:error (str "找不到卡片：" id)}
      (and (some? version) (not= version (:node/version node)))
      {:error (str "卡片已被修改（目前版本 " (:node/version node) "），請先 get_card 重新讀取")}
      (and (nil? title) (nil? intent)) {:error "沒有要修改的欄位：請給 title 或 intent"}
      (and (some? title) (= :intro k)) {:error "引言沒有名稱，不能設定 title"}
      (and (some? title) (not (#{:heading :paragraph} k)))
      {:error (str (kind-names k) "沒有可修改的 title")}
      (and (some? title) (blank? title)) {:error "title 不能是空白"}
      (and (some? intent) (not (string? intent))) {:error "intent 必須是字串"}
      :else
      {:state (cond-> state
                (some? title) (ui/set-node-field id (if (= :heading k) :node/name :node/title) title)
                (some? intent) (ui/set-node-field id :node/draft intent))
       :touched [id]
       :origin :manual
       :label (str "修改" (card-ref nodes id))})))

;; ---------------------------------------------------------------------------
;; 移動

(defn move-card
  "與畫面拖曳同一套規則：把 id 放到 target-id 的 position（:before、:after、:into）。
  移動段落後，相鄰段落的關係標為待確認（tree/move-with-relations）。"
  [state {:keys [id target-id position]}]
  (let [{:keys [nodes]} state
        title (when (contains? nodes id) (card-ref nodes id))]
    (cond
      (not (contains? nodes id)) {:error (str "找不到卡片：" id)}
      (not (contains? nodes target-id)) {:error (str "找不到 target_id：" target-id)}
      (not (#{:before :after :into} position)) {:error "position 只接受 before、after、into"}
      (= id target-id) {:error "不能把卡片移到自己旁邊"}
      (tree/descendant? nodes id target-id) {:error (str "不能把" title "移到自己的下層")}
      :else
      (let [place (tree/placement nodes id target-id position)]
        (if-not place
          {:error (str "不能把" title "移到" (card-ref nodes target-id)
                       ({:before "之前" :after "之後" :into "裡面"} position) "："
                       (or (reject-reason nodes (if (= position :into) target-id (tree/parent nodes target-id)) id)
                           (if (= :intro (tree/kind nodes id))
                             "引言固定在最前面"
                             "引言固定在最前面，其他卡片不能排在它前面")))}
          (let [{ns :nodes rels :relations} (tree/move-with-relations state id place)
                para? (tree/paragraph? nodes id)]
            {:state (assoc state :nodes ns :relations rels)
             :touched [id]
             :origin :move
             :label (str "移動" title)
             :message (str "已將" title "移到" (if-let [p (:parent place)] (card-ref nodes p) "報告最上層")
                           (if para? "；相鄰段落的關係標為「待確認」" "；編號已重新計算"))}))))))

;; ---------------------------------------------------------------------------
;; 關係

(defn set-relation
  "設定兩個相鄰段落（同一標題下前後相接，from 在前）的關係並確認。"
  [state {:keys [from-id to-id type]}]
  (let [{:keys [nodes relations]} state]
    (cond
      (not (contains? nodes from-id)) {:error (str "找不到 from_id：" from-id)}
      (not (contains? nodes to-id)) {:error (str "找不到 to_id：" to-id)}
      (not (some #{type} tree/adjacent-relation-types))
      {:error (str "type 只接受 " (str/join "、" (map name tree/adjacent-relation-types)))}
      (not (and (tree/paragraph? nodes from-id) (tree/paragraph? nodes to-id)))
      {:error "相鄰關係只能設定在兩個段落之間"}
      (not= from-id (tree/prev-paragraph nodes to-id))
      {:error (str (card-ref nodes from-id) "與" (card-ref nodes to-id)
                   "不相鄰：相鄰關係只能設定在同一標題下前後相接的段落（from_id 在前）；不相鄰的段落請改用 add_remote_relation")}
      :else
      (let [existing (tree/relation-between relations from-id to-id)
            same? (and existing (= type (:relation/type existing)) (not (:relation/pending existing)))]
        {:state (if same? state (ui/set-relation state from-id to-id type))
         :touched [from-id to-id]
         :origin :manual
         :label (str "設定段落關係：" (card-ref nodes from-id) "→" (card-ref nodes to-id))
         :note (when same? "這個關係已經存在並已確認，沒有變動")}))))

(defn add-remote-relation
  "新增不相鄰的關係：from 為本段，to 為目標段落（兩者都是段落且不同）。"
  [state {:keys [from-id to-id type]}]
  (let [{:keys [nodes relations]} state]
    (cond
      (not (contains? nodes from-id)) {:error (str "找不到 from_id：" from-id)}
      (not (contains? nodes to-id)) {:error (str "找不到 to_id：" to-id)}
      (not (some #{type} tree/remote-relation-types))
      {:error (str "type 只接受 " (str/join "、" (map name tree/remote-relation-types)))}
      (= from-id to-id) {:error "from_id 與 to_id 不能是同一張卡片"}
      (not (and (tree/paragraph? nodes from-id) (tree/paragraph? nodes to-id)))
      {:error "這種關係只能設定在兩個段落之間"}
      :else
      (let [dup? (some #(and (= from-id (:relation/from %)) (= to-id (:relation/to %))
                             (= type (:relation/type %)) (tree/remote-relation? %))
                       relations)]
        {:state (if dup? state (ui/add-remote-relation state from-id to-id type))
         :touched [from-id to-id]
         :origin :manual
         :label (str "新增關係：" (card-ref nodes from-id) "→" (card-ref nodes to-id))
         :note (when dup? "這個關係已經存在，沒有變動")}))))
