(ns knot.app
  (:require [cljs.reader :as reader]
            [knot.ui :as ui]
            [knot.views :as views]
            [nexus.core :as nexus]
            [replicant.dom :as r]))

(defonce store (atom {}))

;; 純狀態轉換：action 只把結果交給 :store/reset
(def transitions
  {:ui/select ui/select
   :ui/navigate ui/navigate
   :ui/set-view ui/set-view
   :ui/dismiss-toast ui/dismiss-toast
   :ui/move-selected ui/move-selected
   :ui/edge-scroll ui/edge-scroll
   :ui/add-child ui/add-child
   :ui/wrap-paragraphs ui/wrap-paragraphs
   :ui/paragraph->intro ui/paragraph->intro
   :ui/toggle-lock ui/toggle-lock
   :ui/undo ui/undo
   :ui/set-source-filter ui/set-source-filter
   :ui/set-source-query ui/set-source-query
   :drag/end ui/drag-end})

(defn- transition-action [f]
  (fn [state & args]
    (let [state' (apply f state args)]
      (when-not (identical? state state')
        [[:store/reset state']]))))

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
     (fn [state target half]
       (let [state' (ui/drag-over state target half)]
         (cond-> []
           (get-in state' [:ui :over]) (conj [:event/prevent-default])
           (not (identical? state state')) (conj [:store/reset state']))))

     :drag/drop
     (fn [state]
       [[:event/prevent-default]
        [:store/reset (ui/drop-dragged state)]])})

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

    :dom/reveal-columns
    (fn [{:keys [dispatch-data]} _ depth selected n-cols]
      (when-let [el (:replicant/node dispatch-data)]
        (reveal-columns! el depth selected n-cols)))}

   :nexus/placeholders
   {:event/target-value
    (fn [{:replicant/keys [dom-event]}]
      (some-> dom-event .-target .-value))

    ;; 指標在卡片的上半或下半
    :event/half
    (fn [{:replicant/keys [dom-event]}]
      (when dom-event
        (let [rect (.getBoundingClientRect (.-currentTarget dom-event))]
          (if (< (.-clientY dom-event) (+ (.-top rect) (/ (.-height rect) 2))) :top :bottom))))

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
  (when-not (editable-target? e)
    (let [k (.-key e)
          undo? (and (or (.-metaKey e) (.-ctrlKey e)) (not (.-shiftKey e)) (= "z" (.toLowerCase k)))
          dir ({"ArrowLeft" :left "ArrowRight" :right "ArrowUp" :up "ArrowDown" :down} k)]
      (cond
        undo? (do (.preventDefault e) (dispatch! {} [[:ui/undo]]))
        (and dir (= :tree (get-in @store [:ui :view])))
        (do (.preventDefault e) (dispatch! {} [[:ui/navigate dir]]))))))

(defn- viewport-height []
  (-> (- js/window.innerHeight 250) (max 360) (min 1200)))

(defn- on-resize [_]
  (swap! store ui/set-viewport-height (viewport-height)))

(defn- load-sample! []
  (-> (js/fetch "/api/sample-report" #js {:headers #js {"Accept" "application/edn"}})
      (.then (fn [res]
               (if (.-ok res)
                 (.text res)
                 (throw (js/Error. (str "載入範例報告失敗：" (.-status res)))))))
      (.then (fn [text]
               ;; ?select=<id>、?view=fulltext 指定初始畫面，方便對照設計稿與截圖
               (let [params (js/URLSearchParams. js/location.search)
                     select (.get params "select")
                     view (.get params "view")]
                 (reset! store (cond-> (-> (reader/read-string text)
                                           ui/init-state
                                           (ui/set-viewport-height (viewport-height)))
                                 select (ui/select select)
                                 view (ui/set-view (keyword view)))))))
      (.catch (fn [err] (js/console.error err)))))

(defn init []
  (r/set-dispatch! dispatch!)
  (add-watch store ::render (fn [_ _ _ state] (render! state)))
  (js/document.addEventListener "keydown" on-keydown)
  (js/window.addEventListener "resize" on-resize)
  (render! @store)
  (load-sample!))
