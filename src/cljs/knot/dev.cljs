(ns knot.dev
  "開發時在 shadow REPL 檢查畫面的輔助函式（只由 shadow-cljs.edn 的 :preloads 載入，不進正式 build）。
  畫面是 state → hiccup 的函式：送 action、讀 state、在算出的 hiccup 裡找元素，不必截圖。
  會呼叫後端的 action（留言、卡片寫入）一樣會寫進 dev 報告；試寫入請用後端的沙盒。

    (knot.dev/act! [:ui/toggle-todos])
    (knot.dev/texts :div.menu-head)        ;=> (\"未完成的待辦\" ...)
    (knot.dev/find-all :.menu-item)        ; 不限元素名稱
    (knot.dev/dom \".todos-panel\")"
  (:require [clojure.string :as str]
            [knot.app :as app]
            [knot.views :as views]))

(defn state
  "目前的 app state；可帶路徑，例如 (state :ui :selected)。"
  [& path]
  (get-in @app/store path))

(defn act!
  "送出 action（與畫面上的事件相同），回傳之後的 :ui。"
  [& actions]
  (app/dispatch! nil (vec actions))
  (:ui @app/store))

(defn hiccup
  "目前 state 算出的整個畫面。"
  []
  (views/app @app/store))

(defn- parse-tag
  "\"div.a.b\" → [\"div\" #{\"a\" \"b\"}]；\".a\" 的元素名稱是空字串。"
  [s]
  (let [[t & classes] (str/split s #"\.")]
    [t (set classes)]))

(defn- tag-matches?
  "元素名稱相同（want 沒寫名稱時不限），而且含有 want 的所有 class。
  :div.menu-item 符合 :div.menu-item.recent-menu；:.menu-item 也符合 :button.menu-item。"
  [want el]
  (and (vector? el) (keyword? (first el))
       (let [[wt wc] (parse-tag (name want))
             [ht hc] (parse-tag (name (first el)))]
         (and (or (= "" wt) (= wt ht)) (every? hc wc)))))

(defn find-all
  "畫面中所有符合 tag 的元素（hiccup）。"
  ([tag] (find-all tag (hiccup)))
  ([tag h] (filterv #(tag-matches? tag %) (tree-seq coll? seq h))))

(defn find-one
  ([tag] (first (find-all tag)))
  ([tag h] (first (find-all tag h))))

(defn text
  "元素內所有文字串接起來（略過屬性 map）。"
  [el]
  (->> (tree-seq coll? #(if (map? %) nil (seq %)) el)
       (filter string?)
       (apply str)))

(defn texts
  "所有符合 tag 的元素各自的文字。"
  ([tag] (map text (find-all tag)))
  ([tag h] (map text (find-all tag h))))

(defn dom
  "實際 render 到 DOM 的元素數量與文字，用來確認畫面已經更新。"
  [selector]
  (let [els (array-seq (js/document.querySelectorAll selector))]
    {:count (count els) :texts (mapv #(.-textContent %) els)}))
