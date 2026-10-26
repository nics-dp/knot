(ns knot.app
  (:require [cljs.reader :as reader]
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

(defn- reveal-columns!
  "選取時水平捲動，讓選取卡片所在欄與其上、下一欄都在可視範圍內。只在選取改變時捲動。"
  [^js el depth selected n-cols]
  (when (not= selected (.-knotRevealed el))
    (let [first-time? (nil? (.-knotRevealed el))
          _ (set! (.-knotRevealed el) selected)
          col-w (some-> (.querySelector el ".tree-col") .-offsetWidth)
          stride (+ (or col-w 0) views/column-gap)
          start (* stride (max 0 (- depth 2)))
          end (- (* stride (min n-cols (inc depth))) views/column-gap)
          left (.-scrollLeft el)
          width (.-clientWidth el)
          target (cond
                   (or (< start left) (> (- end start) width)) start
                   (> end (+ left width)) (- end width)
                   :else left)]
      (when (not= target left)
        (.scrollTo el #js {:left target
                           :behavior (if (or first-time? (reduced-motion?)) "auto" "smooth")})))))

(declare dispatch! create-sample! undo!)

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

    :server/create-sample
    (fn [_ _] (create-sample!))

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

    :dom/reveal-columns
    (fn [{:keys [dispatch-data]} _ depth selected n-cols]
      (when-let [el (:replicant/node dispatch-data)]
        (reveal-columns! el depth selected n-cols)))}

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
  (when-not (editable-target? e)
    (let [k (.-key e)
          undo? (and (or (.-metaKey e) (.-ctrlKey e)) (not (.-shiftKey e)) (= "z" (.toLowerCase k)))
          dir ({"ArrowLeft" :left "ArrowRight" :right "ArrowUp" :up "ArrowDown" :down} k)
          tree? (= :tree (get-in @store [:ui :page]))]
      (cond
        undo? (do (.preventDefault e) (undo!))
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

(defonce sync-state (atom {:history-count 0 :timer nil :in-flight nil}))

(defn- report-id [] (get-in @store [:report :report/id]))

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
          (if-not (and changes (report-id))
            (js/Promise.resolve nil)
            (let [{:keys [label origin]} (changeset-meta state (:history-count @sync-state))
                  p (-> (api "POST" (str "/api/reports/" (report-id) "/changes")
                             {:label label :origin origin :changes changes})
                        (.then (fn [_]
                                 (reset! saved data)
                                 (swap! sync-state assoc :in-flight nil
                                        :history-count (count (:history state)))
                                 ;; 送出期間又有新的修改
                                 (when (sync/diff @saved (sync/data @store)) (flush!))))
                        (.catch (fn [err]
                                  (swap! sync-state assoc :in-flight nil)
                                  (show-toast! (str "無法儲存：" (ex-message err)))
                                  (js/console.error err))))]
              (swap! sync-state assoc :in-flight p)
              p))))))

(defn- schedule-flush! [old new]
  (when (and @saved (not= (sync/data old) (sync/data new)))
    (if (> (count (:history new)) (count (:history old)))
      (flush!)
      (do (some-> (:timer @sync-state) js/clearTimeout)
          (swap! sync-state assoc :timer (js/setTimeout flush! 800))))))

(defn undo!
  "復原最近一個修改組：先送出尚未儲存的變動，再由後端新增反向的修改組。"
  []
  (when (report-id)
    (-> (flush!)
        (.then #(api "POST" (str "/api/reports/" (report-id) "/undo")))
        (.then (fn [{:keys [label state]}]
                 (if label
                   (do (swap! store #(-> (ui/replace-data % state)
                                         (assoc-in [:ui :toast] (str "已復原：" label))))
                       (reset! saved (sync/data @store))
                       (swap! sync-state assoc :history-count (count (:history @store))))
                   (show-toast! "沒有可以復原的修改"))))
        (.catch (fn [err] (show-toast! (str "無法復原：" (ex-message err))))))))

;; ---------------------------------------------------------------------------
;; 載入

(defn- find-node
  "?select= 可用卡片 id 或名稱（標題、段落名稱）。"
  [nodes s]
  (or (some #(when (= s (str (:node/id %))) (:node/id %)) (vals nodes))
      (some #(when (#{(:node/name %) (:node/title %)} s) (:node/id %))
            (map nodes (tree/document-order nodes)))))

(defn- open-report! [id]
  (-> (api "GET" (str "/api/reports/" id))
      (.then (fn [loaded]
               ;; ?select=、?page=library|tree|fulltext、?drag=、?collapsed=1 指定初始畫面，方便截圖
               (let [params (js/URLSearchParams. js/location.search)
                     select (some->> (.get params "select") (find-node (:nodes (ui/from-server loaded))))
                     page (.get params "page")
                     drag (some->> (.get params "drag") (find-node (:nodes (ui/from-server loaded))))
                     collapsed (.get params "collapsed")
                     state (cond-> (ui/from-server loaded)
                             select (ui/select select)
                             page (ui/set-page (keyword page))
                             drag (ui/drag-start drag)
                             collapsed (ui/toggle-editor))]
                 (reset! saved (sync/data state))
                 (reset! store state))))))

(defn load! []
  (-> (api "GET" "/api/reports")
      (.then (fn [reports]
               (if-let [id (:report/id (first reports))]
                 (open-report! id)
                 (reset! store {:empty true}))))
      (.catch (fn [err]
                (reset! store {:error (ex-message err)})
                (js/console.error err)))))

(defn create-sample! []
  (-> (api "POST" "/api/dev/sample-report")
      (.then (fn [{:report/keys [id]}] (open-report! id)))
      (.catch (fn [err] (reset! store {:error (ex-message err)})))))

(defn init []
  (r/set-dispatch! dispatch!)
  (add-watch store ::render (fn [_ _ _ state] (render! state)))
  (add-watch store ::persist (fn [_ _ old new] (schedule-flush! old new)))
  (js/document.addEventListener "keydown" on-keydown)
  ;; 離開頁面前送出尚未儲存的文字
  (js/window.addEventListener "beforeunload" (fn [_] (flush!)))
  (render! @store)
  (load!))
