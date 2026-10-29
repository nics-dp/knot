(ns knot.prose-editor
  "本文編輯器（SPEC 2、5.4）：Tiptap 核心套件，不依賴 React。
  在 Replicant 的 on-render 生命週期中建立或同步，以自訂 inline node 呈現事實引用與交互參照。
  本文以 ProseMirror JSON 的 EDN 形式存在卡片上（:node/prose）。"
  (:require ["@tiptap/core" :refer [Editor Extension Node wrappingInputRule]]
            ["@tiptap/pm/history" :refer [history redo undo]]
            ["@tiptap/pm/keymap" :refer [keymap]]
            ["@tiptap/pm/schema-list" :refer [liftListItem splitListItem]]))

(def ^:private Doc
  (.create Node #js {:name "doc" :topNode true :content "block+"}))

(def ^:private Paragraph
  (.create Node #js {:name "paragraph" :group "block" :content "inline*"
                     :parseHTML (fn [] #js [#js {:tag "p"}])
                     :renderHTML (fn [^js props] #js ["p" (.-HTMLAttributes props) 0])}))

(def ^:private Text
  (.create Node #js {:name "text" :group "inline"}))

(defn- list-node
  "清單節點（SPEC 5.4「列點」）：不定義 attrs，存回的 JSON 與後端的格式一致（ordered 的 start 等不存）。
  find 為行首的輸入規則（nil 表示不提供）。"
  [name tag content group find]
  (.create Node #js {:name name :group group :content content
                     :parseHTML (fn [] #js [#js {:tag tag}])
                     :renderHTML (fn [^js props] #js [tag (.-HTMLAttributes props) 0])
                     :addInputRules (fn []
                                      (this-as ^js this
                                        (if find
                                          #js [(wrappingInputRule #js {:find find :type (.-type this)})]
                                          #js [])))}))

(def ^:private BulletList (list-node "bulletList" "ul" "listItem+" "block" #"^\s*([-*])\s$"))
(def ^:private OrderedList (list-node "orderedList" "ol" "listItem+" "block" #"^(\d+)\.\s$"))
;; 清單只有一層：listItem 裡只有一個段落，不能再放清單
(def ^:private ListItem (list-node "listItem" "li" "paragraph" nil nil))

(defn- chip-node
  "不可分割的 inline node：name 為 fact、ref 或 cite，attr 為存放 key、label 或區塊 id 的屬性，
  text-fn 由屬性值算出顯示文字；title? 為真時以屬性值作為 title。"
  [name attr css text-fn & [title?]]
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
                                  #js ["span" (clj->js (cond-> {:class css data-attr v :contenteditable "false"}
                                                         title? (assoc :title v)))
                                       (text-fn v)]))})))

(defn- from-pos
  "選取起點的 ResolvedPos。"
  [^js state]
  (.. state -selection -$from))

(defn- in-list-item? [state]
  (= "listItem" (.. ^js (.node ^js (from-pos state) -1) -type -name)))

(defn- empty-item? [state]
  (and (in-list-item? state) (zero? (.. ^js (from-pos state) -parent -content -size))))

(defn- item-start? [^js state]
  (and (.. state -selection -empty) (in-list-item? state) (zero? (.-parentOffset ^js (from-pos state)))))

(defn- list-keys
  "Enter 分出新項目，空項目按 Enter 結束清單；項目開頭按 Backspace 也結束清單。"
  [^js schema]
  (let [item (aget (.-nodes schema) "listItem")
        lift (liftListItem item)
        split (splitListItem item)]
    #js {"Enter" (fn [state dispatch]
                   (if (empty-item? state)
                     (lift state dispatch)
                     (split state dispatch)))
         "Backspace" (fn [state dispatch]
                       (and (item-start? state) (lift state dispatch)))}))

(def ^:private ListKeys
  ;; 優先於預設的 Enter／Backspace
  (.create Extension #js {:name "listKeys" :priority 1000
                          :addProseMirrorPlugins
                          (fn [] (this-as ^js this
                                   #js [(keymap (list-keys (.. this -editor -schema)))]))}))

(def ^:private History
  ;; 編輯器內的 Ctrl／Cmd+Z 用 ProseMirror 自己的復原（SPEC 5.10）
  (.create Extension #js {:name "history"
                          :addProseMirrorPlugins
                          (fn [] #js [(history)
                                      (keymap #js {"Mod-z" undo "Mod-y" redo "Shift-Mod-z" redo})])}))

(defn- extensions [fact-text ref-text cite-text]
  #js [Doc Paragraph Text History BulletList OrderedList ListItem ListKeys
       (chip-node "fact" "key" "chip chip-fact" fact-text)
       (chip-node "ref" "label" "chip chip-ref" ref-text)
       (chip-node "cite" "block" "chip chip-cite" cite-text true)])

(defn- ->js [doc] (clj->js doc))
(defn json->edn
  "編輯器的 JSON 轉成 EDN。ProseMirror 的 node attrs 是沒有原型的物件（Object.create(null)），
  js->clj 不會轉換、會原樣留下而無法送到後端，所以先經過 JSON 變成一般物件。"
  [json]
  (js->clj (.parse js/JSON (.stringify js/JSON json)) :keywordize-keys true))

(defn- ->edn [^js editor] (json->edn (.getJSON editor)))

(defn render!
  "Replicant on-render：el 上還沒有編輯器時建立；已有時同步可否編輯，並在內容被外部改變
  （例如復原）且使用者沒有在輸入時替換內容。on-change 收到新的本文（EDN）。"
  [^js el {:keys [doc editable? on-change fact-text ref-text cite-text]}]
  (if-let [^js editor (.-knotEditor el)]
    (do
      (set! (.-knotOnChange el) on-change)
      (when (not= editable? (.-isEditable editor))
        (.setEditable editor editable? false))
      (when (and (not (.-isFocused editor)) (not= doc (->edn editor)))
        (.setContent (.-commands editor) (->js doc) #js {:emitUpdate false})))
    (let [editor (Editor. #js {:element el
                               :extensions (extensions fact-text ref-text cite-text)
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
