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

;; 結構規則（SPEC 5.3）：標題底下依序是引言、段落、下層標題。
(defn- order-reason
  "moving-id（新卡片為 ::probe）放進 parent-id 的 index 位置而違反排列順序時的說明；合法時為 nil。
  after-id 是新增時指定的 after_id（沒有時為 nil）。"
  [nodes parent-id moving-id index after-id]
  (when-not (tree/order-ok? nodes parent-id moving-id index)
    (let [r (when parent-id (card-ref nodes parent-id))
          sibs (vec (remove #{moving-id} (tree/child-ids nodes parent-id)))
          before (when (pos? index) (nth sibs (dec index)))
          after (when (< index (count sibs)) (nth sibs index))
          rule "標題底下依序是引言、段落、下層標題"]
      (case (if (and after (tree/intro? nodes after) (not= :intro (tree/kind nodes moving-id)))
              :before-intro
              (tree/kind nodes moving-id))
        :paragraph (str rule "，段落要排在下層標題之前，不能排在下層標題之後"
                        (when (and before (tree/heading? nodes before))
                          (str "；要放進" (card-ref nodes before) "請用 position 為 into"
                               (when after-id "（新增時改以它為 parent_id，建立大綱時放進它的 children）"))))
        :heading (str rule "，下層標題要排在這一層所有段落之後"
                      (when r (str "；要放到" r "的最後，"
                                   (if after-id "after_id 請指定最後一個子卡片，或省略 after_id"
                                       "請接在最後一個子卡片之後，或用 position 為 into"))))
        :intro "引言固定在最前面"
        (str rule "，其他卡片不能排在引言前面")))))

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
              :paragraph nil
              :heading (cond
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

(defn- placement-reason
  "move-card 不能移動的原因：先看 parent 能不能容納，再看排列順序。"
  [nodes moving-id target-id position]
  (let [{p :parent index :index} (tree/target-slot nodes moving-id target-id position)]
    (or (reject-reason nodes p moving-id)
        (order-reason nodes p moving-id index nil)
        "不符合層級規則")))

;; ---------------------------------------------------------------------------
;; 新增

(defn- new-node [kind title intent label target-chars]
  (let [draft (or intent "")]
    (cond-> (case kind
              :intro {:node/kind :intro :node/draft draft}
              :paragraph {:node/kind :paragraph :node/title title :node/status :none :node/draft draft}
              :heading {:node/kind :heading :node/name title :node/draft draft}
              (:table :figure) {:node/kind kind :node/title title :node/draft draft :node/label label})
      target-chars (assoc :node/target-chars target-chars))))

(defn- target-chars-error
  "目標字數的檢查：只有標題、段落、引言可設定，且必須是正整數；合法（含沒給）時為 nil。"
  [kind target-chars]
  (cond
    (nil? target-chars) nil
    (not (#{:heading :paragraph :intro} kind)) (str (kind-names kind) "沒有字數，不能設定 target_chars")
    (not (and (integer? target-chars) (pos? target-chars))) "target_chars 必須是正整數（例如 400）"))

(defn- field-error [kind title label target-chars]
  (cond
    (target-chars-error kind target-chars) (target-chars-error kind target-chars)
    (not (#{:heading :paragraph :intro :table :figure} kind)) "kind 只接受 heading、paragraph、intro、table、figure"
    (and (some? label) (not (#{:table :figure} kind))) "label（代稱）只用於表格與圖，標題、段落、引言不需要"
    (and (= kind :intro) (not (str/blank? (when (string? title) title)))) "引言沒有名稱，不需要 title"
    (and (not= kind :intro) (blank? title)) (str "需要 title（" (case kind
                                                                   :heading "標題名稱"
                                                                   :paragraph "段落標題"
                                                                   (str (kind-names kind) "的標題")) "）")))

(defn- insert-card
  "在 parent-id 底下新增一張卡片，接在 after-id 之後（省略時引言放最前面、其他放最後）。
  回傳 {:state :id} 或 {:error}。"
  [state {:keys [parent-id after-id kind title intent label target-chars]}]
  (let [{:keys [nodes]} state
        probe (assoc nodes ::probe {:node/id ::probe :node/kind kind})]
    (or (some->> (field-error kind title label target-chars) (hash-map :error))
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
          (if-let [why (or (reject-reason probe parent-id ::probe)
                           (when after-id
                             (order-reason probe parent-id ::probe
                                           (inc (.indexOf (tree/child-ids nodes parent-id) after-id))
                                           after-id)))]
            {:error why}
            (let [{:keys [label error]} (when (#{:table :figure} kind) (tree/new-label nodes kind label))]
              (if error
                {:error error}
                (let [id (random-uuid)
                      k (if after-id (tree/key-after nodes after-id) (tree/insert-key nodes parent-id kind))
                      nodes' (assoc nodes id (assoc (new-node kind title intent label target-chars)
                                                    :node/id id :node/parent parent-id :node/order-key k))
                      next-para (tree/next-sibling nodes' id)]
                  {:id id
                   :state (-> state
                              (assoc :nodes nodes')
                              ;; 原本接在後面的段落改為承接新段落，關係待確認（同畫面的 add-sibling）
                              (update :relations #(if (= kind :paragraph)
                                                    (tree/mark-pending % nodes' [next-para]
                                                                       {next-para (tree/prev-sibling nodes' id)})
                                                    %)))}))))))))

(defn add-card
  "新增一張卡片。kind 為 :heading、:paragraph、:intro、:table、:figure；parent-id 為 nil 時新增一章。
  表格與圖只能放在段落底下，可給 :label 代稱（見 knot.tree/new-label）；其他卡片給 :label 是錯誤。"
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
          (and (seq children) (not (#{:heading :paragraph} kind)))
          (fail (str (kind-names kind "這種卡片") "不能有 children，只有標題（可有標題、段落、引言）與段落（可有表格、圖）能有下層卡片"))
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
  "一次建立巢狀骨架：items 為 [{:kind :title :intent :label :target-chars :children [...]}]（標題的 children 是標題、段落、引言，
  段落的 children 只能是表格與圖）。
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
  "修改卡片的名稱（title）、意圖（intent）或目標字數（target-chars）；不改本文。
  target-chars 要有這個 key 才修改：正整數為設定，nil 為清除（只有標題、段落、引言有字數）。
  給了 version 且與目前版本不同時不寫入（SPEC 5.13）。"
  [state {:keys [id title intent version] :as args}]
  (let [{:keys [nodes]} state
        node (get nodes id)
        k (:node/kind node)
        target? (contains? args :target-chars)
        target (:target-chars args)]
    (cond
      (nil? node) {:error (str "找不到卡片：" id)}
      (and (some? version) (not= version (:node/version node)))
      {:error (str "卡片已被修改（目前版本 " (:node/version node) "），請先 get_card 重新讀取")}
      (and (nil? title) (nil? intent) (not target?)) {:error "沒有要修改的欄位：請給 title、intent 或 target_chars"}
      (and target? (target-chars-error k target)) {:error (target-chars-error k target)}
      (and target? (nil? target) (not (#{:heading :paragraph :intro} k)))
      {:error (str (kind-names k) "沒有字數，不能設定 target_chars")}
      (and (some? title) (= :intro k)) {:error "引言沒有名稱，不能設定 title"}
      (and (some? title) (not (#{:heading :paragraph} k)))
      {:error (str (kind-names k) "沒有可修改的 title")}
      (and (some? title) (blank? title)) {:error "title 不能是空白"}
      (and (some? intent) (not (string? intent))) {:error "intent 必須是字串"}
      :else
      {:state (cond-> state
                (some? title) (ui/set-node-field id (if (= :heading k) :node/name :node/title) title)
                (some? intent) (ui/set-node-field id :node/draft intent)
                target? (ui/set-target-chars id target))
       :touched [id]
       :origin :manual
       :label (str "修改" (card-ref nodes id))})))

;; ---------------------------------------------------------------------------
;; 本文與表格內容（SPEC 5.4）

(defn- version-error [node version]
  (when (and (some? version) (not= version (:node/version node)))
    (str "卡片已被修改（目前版本 " (:node/version node) "），請先 get_card 重新讀取")))

(defn write-prose
  "寫入段落或引言的本文。doc 為 knot.prose/markup->doc 的結果；cite-blocks 是本文 [[cite:...]] 引用的區塊 id（UUID），
  依序加入卡片的依據（已有的保留、不重複）。段落的狀態轉為 :ai；引言不套用這組狀態。
  不論原本的狀態（含 :human）一律寫入，使用者可由歷程比較與復原。給了 version 且不同時不寫入。"
  [state {:keys [id doc cite-blocks version]}]
  (let [{:keys [nodes]} state
        node (get nodes id)
        k (:node/kind node)]
    (cond
      (nil? node) {:error (str "找不到卡片：" id)}
      (= :table k) {:error "表格的內容請用 write_table"}
      (not (#{:paragraph :intro} k)) {:error "標題與圖沒有本文，只有段落與引言能用 write_prose"}
      (version-error node version) {:error (version-error node version)}
      (not (map? doc)) {:error "缺少本文"}
      :else
      (let [have (set (map :block (:node/citations node)))
            added (vec (remove have (distinct cite-blocks)))]
        {:state (cond-> (assoc-in state [:nodes id :node/prose] doc)
                  (= :paragraph k) (assoc-in [:nodes id :node/status] :ai)
                  (seq added) (update-in [:nodes id :node/citations] (fnil into []) (map (fn [b] {:block b})) added))
         :touched [id]
         :origin :manual
         :label (if (= :intro k)
                  (str "撰寫「" (:node/name (get nodes (:node/parent node))) "」的引言")
                  (str "撰寫「" (:node/title node) "」"))}))))

(defn- cell-text [cell]
  (cond (string? cell) cell
        (number? cell) (str cell)))

(defn- table-content-error
  "columns 與 rows 不合法時的說明；合法時為 nil。"
  [columns rows]
  (cond
    (not (and (sequential? columns) (seq columns))) "columns 必須是非空的陣列（欄名）"
    (some #(or (not (string? %)) (str/blank? %)) columns) "每個欄名都必須是非空白的字串"
    (not (sequential? rows)) "rows 必須是陣列（可以是空陣列），每一列是儲存格的陣列"
    :else
    (some (fn [[i row]]
            (cond
              (not (sequential? row)) (str "第 " (inc i) " 列必須是儲存格的陣列")
              (not= (count columns) (count row))
              (str "第 " (inc i) " 列有 " (count row) " 個儲存格，但共有 " (count columns) " 欄，每列都要與欄數相同")
              :else
              (some (fn [[j cell]]
                      (when (nil? (cell-text cell))
                        (str "第 " (inc i) " 列第 " (inc j) " 格必須是字串或數字")))
                    (map-indexed vector row))))
          (map-indexed vector rows))))

(defn write-table
  "寫入表格卡片自己的內容 {:columns [欄名] :rows [[儲存格]]}（儲存格的數字轉成字串），狀態轉為 :ai。
  給了 version 且不同時不寫入。"
  [state {:keys [id columns rows version]}]
  (let [{:keys [nodes]} state
        node (get nodes id)]
    (cond
      (nil? node) {:error (str "找不到卡片：" id)}
      (not= :table (:node/kind node)) {:error (str (kind-names (:node/kind node)) "不是表格，write_table 只能寫入表格卡片")}
      (version-error node version) {:error (version-error node version)}
      (table-content-error columns rows) {:error (table-content-error columns rows)}
      :else
      {:state (-> state
                  (assoc-in [:nodes id :node/table-content]
                            {:columns (vec columns) :rows (mapv #(mapv cell-text %) rows)})
                  (assoc-in [:nodes id :node/status] :ai))
       :touched [id]
       :origin :manual
       :label (str "填寫表格「" (:node/title node) "」")})))

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
                       (placement-reason nodes id target-id position))}
          (let [{ns :nodes rels :relations} (tree/move-with-relations state id place)
                para? (tree/paragraph? nodes id)]
            {:state (assoc state :nodes ns :relations rels)
             :touched [id]
             :origin :move
             :label (str "移動" title)
             ;; message 用移動後的編號（舊的 title 只用在錯誤與修改組名稱）
             :message (str "已將" (card-ref ns id) "移到" (if-let [p (:parent place)] (card-ref ns p) "報告最上層")
                           (if para? "；相鄰段落的關係標為「待確認」" "；編號已重新計算"))}))))))

;; ---------------------------------------------------------------------------
;; 刪除

(defn delete-card
  "刪除卡片與所有下層卡片，規則同畫面的刪除（ui/delete-node）：相關的關係一併移除，
  原本接在後面的段落改為承接前一段、關係標為待確認。給了 version 且不同時不寫入。"
  [state {:keys [id version]}]
  (let [{:keys [nodes]} state
        node (get nodes id)]
    (cond
      (nil? node) {:error (str "找不到卡片：" id)}
      (and (some? version) (not= version (:node/version node)))
      {:error (str "卡片已被修改（目前版本 " (:node/version node) "），請先 get_card 重新讀取")}
      (= (set (keys nodes)) (set (cons id (tree/descendant-ids nodes id))))
      {:error "報告至少要保留一張卡片"}
      :else
      (let [title (card-ref nodes id)
            state' (ui/delete-node state id)
            n (count (tree/descendant-ids nodes id))
            next-para (let [s (tree/next-sibling nodes id)] (when (tree/paragraph? nodes s) s))]
        {:state state'
         :touched (if next-para [next-para] [])
         :origin :manual
         :label (str "刪除" title)
         :message (str "已刪除" title (when (pos? n) (str "與 " n " 張下層卡片"))
                       (when next-para (str "；" (card-ref (:nodes state') next-para) "與前一段的關係標為「待確認」"))
                       "；使用者可在 Knot 畫面復原")}))))

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

(defn remove-relation
  "移除 from → to 的關係（相鄰或不相鄰皆可）；給了 type 時只移除該類型。"
  [state {:keys [from-id to-id type]}]
  (let [{:keys [nodes relations]} state
        hit? #(and (= from-id (:relation/from %)) (= to-id (:relation/to %))
                   (or (nil? type) (= type (:relation/type %))))]
    (cond
      (not (contains? nodes from-id)) {:error (str "找不到 from_id：" from-id)}
      (not (contains? nodes to-id)) {:error (str "找不到 to_id：" to-id)}
      (not-any? hit? relations)
      {:error (str (card-ref nodes from-id) "→" (card-ref nodes to-id) "沒有"
                   (if type (str "類型為 " (name type) " 的") "任何") "關係；請用 get_card 查看現有的關係")}
      :else
      {:state (update state :relations #(vec (remove hit? %)))
       :touched [from-id to-id]
       :origin :manual
       :label (str "移除關係：" (card-ref nodes from-id) "→" (card-ref nodes to-id))})))
