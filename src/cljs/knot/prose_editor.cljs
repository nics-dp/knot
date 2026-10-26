(ns knot.prose-editor
  "本文編輯器（SPEC 2、5.4）：Tiptap 核心套件，不依賴 React。
  在 Replicant 的 on-render 生命週期中建立或同步，以自訂 inline node 呈現事實引用與交互參照。
  本文以 ProseMirror JSON 的 EDN 形式存在卡片上（:node/prose）。"
  (:require ["@tiptap/core" :refer [Editor Extension Node]]
            ["@tiptap/pm/history" :refer [history redo undo]]
            ["@tiptap/pm/keymap" :refer [keymap]]))

(def ^:private Doc
  (.create Node #js {:name "doc" :topNode true :content "paragraph+"}))

(def ^:private Paragraph
  (.create Node #js {:name "paragraph" :group "block" :content "inline*"
                     :parseHTML (fn [] #js [#js {:tag "p"}])
                     :renderHTML (fn [^js props] #js ["p" (.-HTMLAttributes props) 0])}))

(def ^:private Text
  (.create Node #js {:name "text" :group "inline"}))

(defn- chip-node
  "不可分割的 inline node：name 為 fact 或 ref，attr 為存放 key 或 label 的屬性，
  text-fn 由屬性值算出顯示文字。"
  [name attr css text-fn]
  (let [data-attr (str "data-" name)]
    (.create Node
             #js {:name name :group "inline" :inline true :atom true :selectable true
                  :addAttributes (fn []
                                   (clj->js {attr {:default nil
                                                   :parseHTML #(.getAttribute % data-attr)
                                                   :renderHTML (fn [_] #js {})}}))
                  :parseHTML (fn [] #js [#js {:tag (str "span[" data-attr "]")}])
                  :renderHTML (fn [^js props]
                                (let [v (aget (.. props -node -attrs) attr)]
                                  #js ["span" (clj->js {:class css data-attr v :contenteditable "false"})
                                       (text-fn v)]))})))

(def ^:private History
  ;; 編輯器內的 Ctrl／Cmd+Z 用 ProseMirror 自己的復原（SPEC 5.10）
  (.create Extension #js {:name "history"
                          :addProseMirrorPlugins
                          (fn [] #js [(history)
                                      (keymap #js {"Mod-z" undo "Mod-y" redo "Shift-Mod-z" redo})])}))

(defn- extensions [fact-text ref-text]
  #js [Doc Paragraph Text History
       (chip-node "fact" "key" "chip chip-fact" fact-text)
       (chip-node "ref" "label" "chip chip-ref" ref-text)])

(defn- ->js [doc] (clj->js doc))
(defn- ->edn [^js editor] (js->clj (.getJSON editor) :keywordize-keys true))

(defn render!
  "Replicant on-render：el 上還沒有編輯器時建立；已有時同步可否編輯，並在內容被外部改變
  （例如復原）且使用者沒有在輸入時替換內容。on-change 收到新的本文（EDN）。"
  [^js el {:keys [doc editable? on-change fact-text ref-text]}]
  (if-let [^js editor (.-knotEditor el)]
    (do
      (set! (.-knotOnChange el) on-change)
      (when (not= editable? (.-isEditable editor))
        (.setEditable editor editable? false))
      (when (and (not (.-isFocused editor)) (not= doc (->edn editor)))
        (.setContent (.-commands editor) (->js doc) #js {:emitUpdate false})))
    (let [editor (Editor. #js {:element el
                               :extensions (extensions fact-text ref-text)
                               :content (->js doc)
                               :editable editable?
                               :editorProps #js {:attributes #js {:class "prose-editable" :aria-label "本文"}}})]
      (set! (.-knotOnChange el) on-change)
      (.on editor "update" (fn [] ((.-knotOnChange el) (->edn editor))))
      (set! (.-knotEditor el) editor))))

(defn destroy! [^js el]
  (when-let [^js editor (.-knotEditor el)]
    (.destroy editor)
    (set! (.-knotEditor el) nil)))
