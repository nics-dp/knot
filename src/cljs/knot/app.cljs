(ns knot.app
  (:require [cljs.reader :as reader]
            [knot.prose-editor :as prose-editor]
            [knot.sync :as sync]
            [knot.tree :as tree]
            [knot.ui :as ui]
            [knot.views :as views]
            [nexus.core :as nexus]
            [replicant.dom :as r]))

(defonce store (atom {}))

;; 純狀態轉換：action 只把結果交給 :store/reset
(def transitions
  {:ui/select ui/select
   :ui/navigate ui/navigate
   :ui/dismiss-toast ui/dismiss-toast
   :ui/toggle-editor ui/toggle-editor
   :ui/open-editor ui/open-editor
   :ui/close-editor ui/close-editor
   :ui/toggle-previous ui/toggle-previous
   :ui/toggle-relations-edit ui/toggle-relations-edit
   :ui/move-selected ui/move-selected
   :ui/edge-scroll ui/edge-scroll
   :ui/scroll-column ui/scroll-column
   :ui/add-sibling ui/add-sibling
   :ui/delete-node ui/delete-node
   :ui/clear-focus-title ui/clear-focus-title
   :ui/set-viewport-height ui/set-viewport-height
   :ui/add-child ui/add-child
   :ui/wrap-paragraphs ui/wrap-paragraphs
   :ui/paragraph->intro ui/paragraph->intro
   :ui/toggle-lock ui/toggle-lock
   :ui/set-source-filter ui/set-source-filter
   :ui/set-source-query ui/set-source-query
   :ui/set-page ui/set-page
   :ui/set-node-field ui/set-node-field
   :ui/set-prose ui/set-prose
   :ui/add-citation ui/add-citation
   :ui/remove-citation ui/remove-citation
   :ui/set-relation ui/set-relation
   :ui/open-picker ui/open-picker
   :ui/close-picker ui/close-picker
   :ui/set-picker-query ui/set-picker-query
   :ui/set-picker-filter ui/set-picker-filter
   :ui/select-block ui/select-block
   :ui/toggle-source-file ui/toggle-source-file
   :ui/set-block-status ui/set-block-status
   :ui/open-paragraph ui/open-paragraph
   :ui/add-remote-relation ui/add-remote-relation
   :ui/remove-remote-relation ui/remove-remote-relation
   :ui/set-remote-relation-type ui/set-remote-relation-type
   :ui/open-rel-picker ui/open-rel-picker
   :ui/close-rel-picker ui/close-rel-picker
   :ui/set-rel-picker-type ui/set-rel-picker-type
   :ui/set-rel-picker-query ui/set-rel-picker-query
   :drag/end ui/drag-end})

(defn- transition-action [f]
  (fn [state & args]
    ;; 中文輸入法選字中（isComposing）不更新，等 compositionend 再寫入
    (when-not (some #{::composing} args)
      (let [state' (apply f state args)]
        (when-not (identical? state state')
          [[:store/reset state']])))))

(defn- reduced-motion? []
  (.-matches (js/window.matchMedia "(prefers-reduced-motion: reduce)")))

(defn- obscured-width
  "編輯區以抽屜覆蓋在看板上時，它遮住的寬度；收合或窄螢幕（不覆蓋）時為 0。"
  []
  (let [ed (js/document.querySelector ".editor")]
    (if (and ed (= "absolute" (.-position (js/getComputedStyle ed))))
      (.-offsetWidth ed)
      0)))

(defn- reveal-columns!
  "選取時水平捲動，讓選取卡片所在欄與其上、下一欄都在未被編輯區遮住的範圍內。
  只在選取或編輯區開合改變時捲動。"
  [^js el depth selected n-cols collapsed?]
  (let [k (str selected "|" collapsed?)]
    (when (not= k (.-knotRevealed el))
      (let [first-time? (nil? (.-knotRevealed el))
            _ (set! (.-knotRevealed el) k)
            col-w (some-> (.querySelector el ".tree-col") .-offsetWidth)
            stride (+ (or col-w 0) views/column-gap)
            start (* stride (max 0 (- depth 2)))
            end (- (* stride (min n-cols (inc depth))) views/column-gap)
            left (.-scrollLeft el)
            width (max stride (- (.-clientWidth el) (obscured-width)))
            target (cond
                     (or (< start left) (> (- end start) width)) start
                     (> end (+ left width)) (- end width)
                     :else left)]
        (when (not= target left)
          (.scrollTo el #js {:left target
                             :behavior (if (or first-time? (reduced-motion?)) "auto" "smooth")}))))))

(declare dispatch! import-sample! undo! load-previous!)

(def nexus
  {:nexus/system->state deref

   :nexus/actions
   (merge
    (update-vals transitions transition-action)
    {:drag/start
     (fn [state id]
       [[:event/set-drag-data id]
        [:store/reset (ui/drag-start state id)]])

     :drag/over
     (fn [state column viewport-y translate]
       (let [state' (ui/drag-over state column (some-> viewport-y (- translate)))]
         (cond-> []
           (get-in state' [:ui :over]) (conj [:event/prevent-default])
           (not (identical? state state')) (conj [:store/reset state']))))

     :drag/drop
     (fn [state]
       [[:event/prevent-default]
        [:store/reset (ui/drop-dragged state)]])

     ;; 滾輪在卡片欄位上只捲動該欄，不捲動整頁
     :ui/wheel-column
     (fn [_ column delta lo hi]
       [[:event/prevent-default]
        [:ui/scroll-column column delta lo hi]])})

   :nexus/effects
   {:store/reset
    (fn [_ store state] (reset! store state))

    :event/prevent-default
    (fn [{:keys [dispatch-data]} _]
      (some-> (:replicant/dom-event dispatch-data) .preventDefault))

    :event/set-drag-data
    (fn [{:keys [dispatch-data]} _ id]
      (when-let [dt (some-> (:replicant/dom-event dispatch-data) .-dataTransfer)]
        (set! (.-effectAllowed dt) "move")
        (.setData dt "text/plain" id)))

    :server/import-sample
    (fn [_ _] (import-sample!))

    :dom/focus
    (fn [{:keys [dispatch-data]} _]
      (some-> (:replicant/node dispatch-data) .focus))

    :dom/select-all
    (fn [{:keys [dispatch-data]} _]
      (when-let [el (:replicant/node dispatch-data)]
        (.focus el)
        (.select el)))

    ;; 卡片欄位的可視高度由 CSS 決定；量測後交給版面計算選取置中
    :dom/observe-viewport
    (fn [_ _]
      (let [measure (fn []
                      (when-let [vp (js/document.querySelector ".col-viewport")]
                        (let [h (.-clientHeight vp)]
                          (when (and (pos? h) (not= h (get-in @store [:ui :viewport-h])))
                            (dispatch! {} [[:ui/set-viewport-height h]])))))]
        (when-let [el (js/document.querySelector ".tree-scroll")]
          (.observe (js/ResizeObserver. measure) el))
        (measure)))

    ;; 本文編輯器：建立或同步（內容取自目前的狀態）
    :prose/render
    (fn [{:keys [dispatch-data]} _ id editable?]
      (when-let [el (:replicant/node dispatch-data)]
        (prose-editor/render!
         el {:doc (get-in @store [:nodes id :node/prose])
             :editable? editable?
             :on-change #(dispatch! {} [[:ui/set-prose id %]])
             :fact-text (fn [k]
                          (let [{:fact/keys [value unit]} (get-in @store [:facts k])]
                            (if value (str value unit) k)))
             :ref-text views/xref-text})))

    :prose/destroy
    (fn [{:keys [dispatch-data]} _]
      (some-> (:replicant/node dispatch-data) prose-editor/destroy!))

    ;; 全文頁：選取改變時，把選取的標題或段落捲到可視範圍（使用者自己點的已在畫面中，不會移動）
    :dom/reveal-selected
    (fn [{:keys [dispatch-data]} _ selected]
      (when-let [^js el (:replicant/node dispatch-data)]
        (when (not= selected (.-knotRevealed el))
          (let [first-time? (nil? (.-knotRevealed el))]
            (set! (.-knotRevealed el) selected)
            (when-let [target (.querySelector el (str "[data-id=\"" selected "\"]"))]
              ;; 剛進入全文頁時直接置中；之後切換選取時平順捲到最近的位置
              (.scrollIntoView target (if first-time?
                                        #js {:block "center" :behavior "auto"}
                                        #js {:block "nearest"
                                             :behavior (if (reduced-motion?) "auto" "smooth")})))))))

    :dom/reveal-columns
    (fn [{:keys [dispatch-data]} _ depth selected n-cols collapsed?]
      (when-let [el (:replicant/node dispatch-data)]
        (reveal-columns! el depth selected n-cols collapsed?)))}

   :nexus/placeholders
   {:event/target-value
    (fn [{:replicant/keys [dom-event]}]
      (if (some-> dom-event .-isComposing)
        ::composing
        (some-> dom-event .-target .-value)))

    ;; <select> 的值轉為 keyword；空字串為 nil
    :event/target-keyword
    (fn [{:replicant/keys [dom-event]}]
      (let [v (some-> dom-event .-target .-value)]
        (when-not (or (nil? v) (= "" v)) (keyword v))))

    ;; 滾輪往下時內容往上移
    :event/wheel-delta
    (fn [{:replicant/keys [dom-event]}]
      (when dom-event
        (let [unit (case (.-deltaMode dom-event) 1 16 2 400 1)]
          (- (* unit (.-deltaY dom-event))))))

    ;; 指標在事件目標（欄位）內的垂直位置
    :event/viewport-y
    (fn [{:replicant/keys [dom-event]}]
      (when dom-event
        (- (.-clientY dom-event) (.-top (.getBoundingClientRect (.-currentTarget dom-event))))))

    ;; 拖曳時指標靠近欄位上下緣：往下捲為負、往上捲為正
    :event/edge-delta
    (fn [{:replicant/keys [dom-event]}]
      (when dom-event
        (let [rect (.getBoundingClientRect (.-currentTarget dom-event))
              y (- (.-clientY dom-event) (.-top rect))]
          (cond (< y 56) 12
                (> y (- (.-height rect) 56)) -12
                :else 0))))}})

(defn dispatch! [dispatch-data actions]
  (nexus/dispatch nexus store dispatch-data actions))

(defn render! [state]
  (r/render (js/document.getElementById "app") (views/app state)))

(defn reload! []
  (render! @store))

(defn- editable-target? [^js e]
  (let [t (.-target e)]
    (or (#{"INPUT" "TEXTAREA" "SELECT"} (.-tagName t))
        (.-isContentEditable t))))

(defn- on-keydown [^js e]
  ;; 卡片本體是 div：Enter／空白鍵選取
  (when-let [id (and (#{"Enter" " "} (.-key e))
                     (some-> (.-target e) (.closest ".card-body") (.getAttribute "data-id")))]
    (.preventDefault e)
    (dispatch! {} [[:ui/select id]]))
  ;; 在意圖或本文中按 Esc：結束編輯（離開輸入框），再按一次才收合編輯區
  (when (and (= "Escape" (.-key e)) (editable-target? e))
    (.blur (.-target e)))
  (when-not (editable-target? e)
    (let [k (.-key e)
          undo? (and (or (.-metaKey e) (.-ctrlKey e)) (not (.-shiftKey e)) (= "z" (.toLowerCase k)))
          dir ({"ArrowLeft" :left "ArrowRight" :right "ArrowUp" :up "ArrowDown" :down} k)
          tree? (= :tree (get-in @store [:ui :page]))]
      (cond
        undo? (do (.preventDefault e) (undo!))
        (= "Escape" k) (dispatch! {} [[:ui/close-editor]])
        ;; Alt+↑／↓：上移、下移（拖曳的鍵盤替代）
        (and tree? (.-altKey e) (#{:up :down} dir))
        (do (.preventDefault e) (dispatch! {} [[:ui/move-selected (if (= dir :up) -1 1)]]))
        (and dir tree?)
        (do (.preventDefault e) (dispatch! {} [[:ui/navigate dir]]))))))

;; ---------------------------------------------------------------------------
;; 與後端同步：前端算出新狀態後，只把變動的實體送到後端（knot.sync/diff）。
;; 結構類的修改（有記入 :history）立即送出；逐字輸入在停止約 0.8 秒後送出。

(defonce saved
  ;; 後端已有的資料（sync/data 的形式）
  (atom nil))

(defonce sync-state
  ;; :own 是自己送出的修改組 id，收到它們的事件時不必重新載入；
  ;; :reload-pending 表示有其他來源的修改，等沒有未送出的輸入時再重新載入
  (atom {:history-count 0 :timer nil :in-flight nil :own #{} :reload-pending false}))

(defn- loaded? [] (some? (:report @store)))

(defn- api [method path & [body]]
  (-> (js/fetch path (clj->js (cond-> {:method method
                                       :headers {"Accept" "application/edn"}}
                                body (-> (assoc :body (pr-str body))
                                         (assoc-in [:headers "Content-Type"] "application/edn")))))
      (.then (fn [res]
               (-> (.text res)
                   (.then (fn [text]
                            (let [data (when (seq text) (reader/read-string text))]
                              (if (.-ok res)
                                data
                                (throw (ex-info (or (:error data) (str "HTTP " (.-status res))) data)))))))))))

(defn- show-toast! [msg]
  (swap! store assoc-in [:ui :toast] msg))

(defn- changeset-meta
  "這次送出的修改組名稱與來源：取自上次送出後新增的 :history，沒有時視為文字編輯。"
  [state from-count]
  (let [entries (subvec (vec (:history state)) (min from-count (count (:history state))))]
    (if-let [last-entry (peek entries)]
      {:label (:label last-entry)
       :origin (if (some #(= :move (:origin %)) entries) :move :manual)}
      {:label "編輯文字" :origin :manual})))

(declare reload-if-idle!)

(defn- node-names [state ids]
  (let [title (let [numbers (tree/numbering (:nodes state))]
                #(tree/display-title (:nodes state) numbers %))]
    (->> ids (filter (:nodes state)) (map #(str "「" (title %) "」")) (apply str))))

(defn- on-conflict!
  "版本衝突：這次的修改沒有寫入。重新載入後端的資料，並說明哪些卡片被其他來源改過。"
  [conflicts]
  (let [names (node-names @store (map :node/id conflicts))]
    (swap! sync-state assoc :reload-pending true)
    (-> (reload-if-idle! {:force? true})
        (.then #(show-toast! (str names "剛被其他來源（agent 或其他分頁）修改，已重新載入；剛才的修改沒有存入"))))))

(defn flush!
  "把尚未送出的變動送到後端；回傳 Promise。同一時間只送一個修改組。"
  []
  (let [{:keys [in-flight timer]} @sync-state]
    (when timer (js/clearTimeout timer))
    (swap! sync-state assoc :timer nil)
    (or in-flight
        (let [state @store
              data (sync/data state)
              changes (sync/diff @saved data)]
          (if-not (and changes (loaded?))
            (js/Promise.resolve nil)
            (let [{:keys [label origin]} (changeset-meta state (:history-count @sync-state))
                  p (-> (api "POST" "/api/report/changes"
                             {:label label :origin origin :changes changes})
                        (.then (fn [{:keys [versions] :as res}]
                                 ;; 後端寫入後的版本同時寫回已存檔的資料與目前狀態，下次送出才不會與自己衝突
                                 (reset! saved (ui/apply-versions data versions))
                                 (swap! store ui/apply-versions versions)
                                 (swap! sync-state #(cond-> (assoc % :in-flight nil
                                                                   :history-count (count (:history state)))
                                                      (:changeset/id res) (update :own conj (:changeset/id res))))
                                 (when (get-in @store [:ui :show-previous]) (load-previous!))
                                 ;; 送出期間又有新的修改
                                 (if (sync/diff @saved (sync/data @store))
                                   (flush!)
                                   (reload-if-idle!))))
                        (.catch (fn [err]
                                  (swap! sync-state assoc :in-flight nil)
                                  (if-let [conflicts (:conflicts (ex-data err))]
                                    (on-conflict! conflicts)
                                    (show-toast! (str "無法儲存：" (ex-message err))))
                                  (js/console.error err))))]
              (swap! sync-state assoc :in-flight p)
              p))))))

(defn- schedule-flush! [old new]
  (when (and @saved (not= (sync/data old) (sync/data new)))
    (if (> (count (:history new)) (count (:history old)))
      (flush!)
      (do (some-> (:timer @sync-state) js/clearTimeout)
          (swap! sync-state assoc :timer (js/setTimeout flush! 800))))))

(defn- load-previous!
  "讀回選取卡片的前一版內容，供本文區「與前一版比較」。"
  []
  (let [id (get-in @store [:ui :selected])]
    (-> (api "GET" (str "/api/report/nodes/" id "/previous"))
        (.then (fn [data] (swap! store ui/set-previous id data)))
        (.catch (fn [err] (show-toast! (str "無法讀取前一版：" (ex-message err))))))))

(defn- watch-previous!
  "打開比較、或比較打開時換了卡片，就重新讀取前一版。"
  [old new]
  (let [on? (get-in new [:ui :show-previous])]
    (when (and on? (loaded?)
               (or (not (get-in old [:ui :show-previous]))
                   (not= (get-in old [:ui :selected]) (get-in new [:ui :selected]))))
      (load-previous!))))

(defn undo!
  "復原最近一個修改組：先送出尚未儲存的變動，再由後端新增反向的修改組。"
  []
  (when (loaded?)
    (-> (flush!)
        (.then #(api "POST" "/api/report/undo"))
        (.then (fn [{:keys [label state]}]
                 (if label
                   (do (swap! store #(-> (ui/replace-data % state)
                                         (assoc-in [:ui :toast] (str "已復原：" label))))
                       (reset! saved (sync/data @store))
                       (swap! sync-state assoc :history-count (count (:history @store))))
                   (show-toast! "沒有可以復原的修改"))))
        (.catch (fn [err] (show-toast! (str "無法復原：" (ex-message err))))))))

;; ---------------------------------------------------------------------------
;; 其他來源的修改（SPEC 5.13）：後端以 Server-Sent Events 通知；沒有未送出的輸入時重新載入

(defn- dirty? []
  (let [{:keys [timer in-flight]} @sync-state]
    (or timer in-flight (some? (sync/diff @saved (sync/data @store))))))

(defn reload-if-idle!
  "有待重新載入的修改、且沒有未送出的輸入時，讀回整份報告並保留畫面狀態。
  :force? 用於衝突：捨棄未送出的變動。回傳 Promise。"
  ([] (reload-if-idle! {}))
  ([{:keys [force?]}]
   (if-not (and (loaded?) (:reload-pending @sync-state) (or force? (not (dirty?))))
     (js/Promise.resolve nil)
     (-> (api "GET" "/api/report")
         (.then (fn [loaded]
                  ;; 讀取期間使用者又有輸入時先不取代，等送出後再試
                  (when (or force? (not (dirty?)))
                    (swap! store ui/replace-data loaded)
                    (reset! saved (sync/data @store))
                    (swap! sync-state assoc :reload-pending false
                           :history-count (count (:history @store))))))
         (.catch (fn [err] (js/console.error err)))))))

(defn- on-change-event [e]
  (let [{:keys [type] :as event} (reader/read-string (.-data e))]
    ;; 自己的修改組的事件可能比 POST 的回應先到，等送出完成、記下 id 後再判斷
    (-> (or (:in-flight @sync-state) (js/Promise.resolve nil))
        (.then (fn [_]
                 (when-not (and (= :change type) (contains? (:own @sync-state) (:changeset/id event)))
                   (swap! sync-state assoc :reload-pending true)
                   (reload-if-idle!)))))))

(defonce events
  ;; EventSource 斷線後會自動重連
  (atom nil))

(defn- listen-events! []
  (when-not @events
    (let [es (js/EventSource. "/api/events")]
      (.addEventListener es "change" on-change-event)
      (reset! events es))))

;; ---------------------------------------------------------------------------
;; 載入

(defn- find-node
  "?select= 可用卡片 id 或名稱（標題、段落名稱）。"
  [nodes s]
  (or (some #(when (= s (str (:node/id %))) (:node/id %)) (vals nodes))
      (some #(when (#{(:node/name %) (:node/title %)} s) (:node/id %))
            (map nodes (tree/document-order nodes)))))

(defn- show-report! [loaded]
  ;; ?select=、?page=library|tree|fulltext、?drag=、?editor=1（展開編輯區）、?previous=1（與前一版比較）
  ;; 指定初始畫面，方便截圖
  (let [params (js/URLSearchParams. js/location.search)
        nodes (:nodes (ui/from-server loaded))
        select (some->> (.get params "select") (find-node nodes))
        page (.get params "page")
        drag (some->> (.get params "drag") (find-node nodes))
        editor (.get params "editor")
        previous (.get params "previous")
        state (cond-> (ui/from-server loaded)
                select (ui/select select)
                page (ui/set-page (keyword page))
                drag (ui/drag-start drag)
                editor (ui/open-editor)
                previous (ui/toggle-previous))]
    (reset! saved (sync/data state))
    (reset! store state)
    (when previous (load-previous!))))

(defn load! []
  (-> (api "GET" "/api/report")
      (.then show-report!)
      (.catch (fn [err]
                (reset! store {:error (ex-message err)})
                (js/console.error err)))))

(defn import-sample!
  "開發用：把範例報告匯入空白報告。"
  []
  (-> (api "POST" "/api/dev/sample")
      (.then show-report!)
      (.catch (fn [err] (show-toast! (str "無法匯入範例：" (ex-message err)))))))

(defn init []
  (r/set-dispatch! dispatch!)
  (add-watch store ::render (fn [_ _ _ state] (render! state)))
  (add-watch store ::persist (fn [_ _ old new] (schedule-flush! old new)))
  (add-watch store ::previous (fn [_ _ old new] (watch-previous! old new)))
  (js/document.addEventListener "keydown" on-keydown)
  ;; 離開頁面前送出尚未儲存的文字
  (js/window.addEventListener "beforeunload" (fn [_] (flush!)))
  (render! @store)
  (listen-events!)
  (load!))
