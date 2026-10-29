(ns knot.views-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [knot.ui :as ui]
            [knot.views :as views]))

(deftest app-test
  (is (= :main.app (first (views/app {})))))

(def state
  (ui/init-state
   {:report/title "測試報告"
    :ui/selected "a1"
    :tree [{:node/id "A" :node/kind :heading :node/name "甲"
            :children [{:node/id "a1" :node/kind :paragraph :node/title "一" :node/status :ai
                        :node/prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "本文"}]}]}}
                       {:node/id "a2" :node/kind :paragraph :node/title "二"}]}]}))

(defn- render [state] (pr-str (views/app state)))

(defn- find-all [tag hiccup]
  (filter #(and (vector? %) (= tag (first %))) (tree-seq coll? seq hiccup)))

(defn- find-el [tag hiccup]
  (first (find-all tag hiccup)))

(deftest editor-zones-test
  (testing "預設收合，只顯示把手"
    (is (str/includes? (render state) "editor-rail"))
    (is (not (str/includes? (render state) "生成依據"))))
  (let [html (render (ui/open-editor state))]
    (testing "段落編輯區依序是生成依據、撰寫時參考的內容、本文"
      (let [idx #(str/index-of html %)]
        (is (< (idx "生成依據") (idx "撰寫時參考的內容") (idx "與前一版比較")))))
    (testing "Knot 不生成本文：沒有生成按鈕，交代 agent 用留言"
      (is (not (str/includes? html "生成本段")))
      (is (not (str/includes? html "AI"))))
    (testing "不再有上移、下移按鈕"
      (is (not (str/includes? html "上移"))))
    (testing "改為引言在「⋯」選單"
      (is (str/includes? html "more-menu"))
      (is (str/includes? html "改為引言"))
      (is (not (str/includes? html "[:div.menu [[")) "選單內容是元素，不是一組向量")))
  (testing "關係預設唯讀，按「編輯」才能修改"
    (let [view (render (ui/open-editor state))
          edit (render (-> state ui/open-editor ui/toggle-relations-edit))]
      (is (not (str/includes? view "關係類型")))
      (is (str/includes? edit "關係類型"))
      (is (str/includes? edit "編輯中"))))
  (testing "與前一版比較"
    (let [s (-> state ui/open-editor ui/toggle-previous
                (ui/set-previous "a1" {:node/prose {:value {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "舊文"}]}]}}}))
          html (render s)]
      (is (str/includes? html ":del"))
      (is (str/includes? html "舊"))
      (is (str/includes? html ":ins")))))

(deftest fulltext-page-test
  (let [html (render (ui/set-page state :fulltext))]
    (testing "全文頁有大綱，列出標題"
      (is (str/includes? html "outline"))
      (is (str/includes? html "第一章 甲")))
    (testing "編輯區固定在右側，即使卡片頁收合也顯示"
      (is (true? (get-in state [:ui :editor-collapsed])))
      (is (str/includes? html "editor-docked"))
      (is (not (str/includes? html "editor-rail")))
      (is (not (str/includes? html "收合編輯區"))))))

(def plan-state
  (ui/init-state
   {:report/title "測試報告"
    :ui/selected "p1"
    :tree [{:node/id "H" :node/kind :heading :node/name "甲" :node/draft "本章要交代緣起"
            :children [{:node/id "p1" :node/kind :paragraph :node/title "背景" :node/draft "說明試辦的背景"}
                       {:node/id "p2" :node/kind :paragraph :node/title "目標"}
                       {:node/id "p3" :node/kind :paragraph :node/title "已寫"
                        :node/prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "已有本文"}]}]}}]}]}))

(defn- plan-view [state]
  (find-all :div.ft-plan (views/app (ui/set-page state :fulltext))))

(deftest fulltext-plan-test
  (let [plans (plan-view plan-state)
        text (pr-str plans)]
    (testing "沒有本文的段落顯示名稱與意圖；意圖空白時有說明；有本文的不顯示"
      (is (= 3 (count plans)) "標題意圖、兩個沒有本文的段落")
      (is (str/includes? text "背景 · 尚未撰寫"))
      (is (str/includes? text "說明試辦的背景"))
      (is (str/includes? text "目標 · 尚未撰寫"))
      (is (str/includes? text "（尚未撰寫，也還沒有意圖）"))
      (is (not (str/includes? text "已寫 · 尚未撰寫"))))
    (testing "標題下顯示本章意圖"
      (is (str/includes? text "本章意圖"))
      (is (str/includes? text "本章要交代緣起"))))
  (testing "切換有「顯示意圖」，預設勾選"
    (let [toggle (find-el :label.ft-toggle (views/app (ui/set-page plan-state :fulltext)))]
      (is (str/includes? (pr-str toggle) "顯示意圖"))
      (is (str/includes? (pr-str toggle) ":checked true"))
      (is (str/includes? (pr-str toggle) "[:ui/toggle-fulltext-intents]"))))
  (testing "關閉後：沒有 ft-plan，標題意圖隱藏，段落回到簡短說明"
    (let [off (ui/toggle-fulltext-intents (ui/set-page plan-state :fulltext))
          html (render off)]
      (is (empty? (find-all :div.ft-plan (views/app off))))
      (is (str/includes? html "（尚未撰寫：目標）"))
      (is (not (str/includes? (pr-str (find-el :div.fulltext-page (views/app off))) "本章要交代緣起")))
      (is (str/includes? (pr-str (find-el :label.ft-toggle (views/app off))) ":checked false"))))
  (testing "引言沒有本文：有意圖顯示意圖，沒有意圖維持說明"
    (let [mk (fn [draft] (ui/init-state {:report/title "t" :tree [{:node/id "H" :node/kind :heading :node/name "甲"
                                                                   :children [{:node/id "i" :node/kind :intro :node/draft draft}
                                                                              {:node/id "p" :node/kind :paragraph :node/title "段"}]}]}))
          with (pr-str (plan-view (mk "概述本章")))
          without (render (ui/set-page (mk nil) :fulltext))]
      (is (str/includes? with "引言 · 尚未撰寫"))
      (is (str/includes? with "概述本章"))
      (is (str/includes? without "引言尚未生成")))))

(deftest outline-title-test
  (let [items (find-all :button.outline-item (views/app (ui/set-page plan-state :fulltext)))]
    (is (= 1 (count items)))
    (is (= "第一章 甲" (:title (second (first items)))))))

(deftest report-edit-view-test
  (let [named (assoc plan-state :report {:report/title "共享單車" :report/subtitle "成果報告"})
        top (fn [s] (find-el :header.topbar (views/app s)))]
    (testing "頂部列的名稱是按鈕，按下開啟編輯"
      (let [btn (find-el :button.topbar-title (top named))]
        (is (some? btn))
        (is (= "編輯報告名稱與副標" (:title (second btn))))
        (is (str/includes? (pr-str btn) "[:ui/open-report-edit]"))
        (is (str/includes? (pr-str btn) "共享單車"))
        (is (str/includes? (pr-str btn) "成果報告")))
      (is (nil? (find-el :div.report-edit-panel (top named))) "預設不顯示面板"))
    (testing "面板有名稱與副標輸入框、取消與儲存"
      (let [panel (pr-str (find-el :div.menu-panel.report-edit-panel (top (ui/open-report-edit named))))]
        (is (str/includes? panel ":value \"共享單車\""))
        (is (str/includes? panel ":value \"成果報告\""))
        (is (str/includes? panel "[:report/keydown [:event/key]]"))
        (is (str/includes? panel "[:report/save]"))
        (is (str/includes? panel "[:ui/close-report-edit]"))
        (is (not (str/includes? panel ":disabled true")))))
    (testing "名稱空白時儲存 disabled"
      (let [s (-> named ui/open-report-edit (ui/set-report-draft :title " "))
            save (->> (find-all :button.btn.btn-primary (top s))
                      (filter #(str/includes? (pr-str %) "[:report/save]")) first)]
        (is (true? (:disabled (second save))))))))

(deftest agent-change-test
  (let [s (ui/agent-changed state {:label "建立大綱：3 張卡片" :nodes ["a2"]})
        html (render s)]
    (testing "agent 修改的卡片標示，再次修改時換成另一個動畫"
      (is (str/includes? html "card-agent-b"))
      (is (str/includes? (render (ui/agent-changed s {:label "x" :nodes ["a2"]})) "card-agent-a")))
    (is (not (str/includes? (render state) "card-agent")) "沒有 agent 修改時不標示")))

(deftest recent-menu-test
  (testing "頂部列有「最近修改」，每一頁都有，清單預設關閉"
    (let [html (render state)]
      (is (str/includes? html "[:recent/toggle]"))
      (is (not (str/includes? html "recent-menu"))))
    (is (str/includes? (render (ui/set-page state :fulltext)) "[:recent/toggle]")))
  (let [html (render (-> state
                         (ui/set-recent-cards [{:node/id "a2" :label "修改「二」" :by :agent}
                                               {:node/id "a1" :label "編輯文字" :by :user}
                                               {:node/id "gone" :label "已刪除" :by :user}])
                         ui/toggle-recent))]
    (testing "列出仍存在的卡片與修改者，點選跳到卡片"
      (is (str/includes? html "[:ui/open-recent-card \"a2\"]"))
      (is (str/includes? html "修改「二」"))
      (is (str/includes? html "by-agent"))
      (is (str/includes? html "by-user"))
      (is (not (str/includes? html "gone")))))
  (is (str/includes? (render (ui/toggle-recent state)) "還沒有修改過的卡片")))

(deftest conflict-view-test
  (let [html (render (ui/show-conflict state "「一」" [{:id "a1" :title "一" :field "意圖" :text "沒存到的意圖"}]))]
    (is (str/includes? html "剛被其他來源"))
    (is (str/includes? html "沒存到的意圖"))
    (is (str/includes? html ":clipboard/copy"))))

(def note-state
  (ui/set-notes (ui/open-editor state)
                [{:note/id "t1" :note/node "a1" :note/author :user :note/text "寫短一點" :note/todo true :note/state :open :note/at 1}
                 {:note/id "r1" :note/node "a1" :note/author :agent :note/text "好的" :note/reply-to "t1" :note/at 2}
                 {:note/id "t2" :note/node "a1" :note/author :user :note/text "補數據" :note/todo true :note/state :done
                  :note/changeset "cs" :note/at 3}
                 {:note/id "q1" :note/node "a2" :note/author :agent :note/text "意圖不清楚？" :note/at 4}]))

(deftest notes-zone-test
  (let [html (render note-state)
        idx #(str/index-of html %)]
    (testing "留言與歷程是編輯區最後一個區塊，最新的在下方"
      (is (< (idx "與前一版比較") (idx "留言與歷程")))
      (is (< (idx "寫短一點") (idx "好的") (idx "補數據"))))
    (testing "待辦有勾選框；完成的待辦可以看修改"
      (is (str/includes? html "[:notes/set-state \"t1\" :done]"))
      (is (str/includes? html "[:notes/set-state \"t2\" :open]"))
      (is (str/includes? html "[:ui/open-change \"a1\" \"cs\"]")))
    (testing "只能刪除使用者的留言"
      (is (str/includes? html "[:notes/delete \"t1\"]"))
      (is (not (str/includes? html "[:notes/delete \"r1\"]"))))
    (testing "卡片上的待辦數"
      (is (str/includes? html "1 則未完成的待辦")))))

(deftest todos-menu-test
  (is (str/includes? (render note-state) "[:span.count-badge 2]") "未完成的待辦與 agent 的提問")
  (let [html (render (ui/toggle-todos note-state))]
    (is (str/includes? html "未完成的待辦"))
    (is (str/includes? html "[:ui/open-note-card \"a2\"]") "agent 的提問")
    (is (str/includes? html "整份報告的留言"))
    (is (str/includes? html "compose-:report"))))

(deftest selected-path-test
  (testing "選取卡片的位置在卡片區，頂部列只有報告名稱"
    (let [path (find-el :div.selected-path (views/app state))]
      (is (= [:span.path-current "一"] (last path)))
      (is (str/includes? (pr-str path) "甲 ›")))
    (is (not (str/includes? (pr-str (find-el :header.topbar (views/app state))) "甲")))
    (is (nil? (find-el :div.selected-path (views/app (ui/set-page state :library)))))))

(defn- ts [n] #?(:clj (java.util.Date. (long n)) :cljs (js/Date. n)))

(def history-entries
  [{:changeset-ids [1] :at (ts 0) :label "建立卡片" :by :user :kind :create :fields #{} :merged 1}
   {:changeset-ids [2 3] :at (ts 2.5) :label "編輯文字" :by :user :kind :change :fields #{:prose :status} :merged 2}
   {:changeset-ids [4] :at (ts 10) :label "" :by :agent :kind :change
    :fields #{:name :intent}
    :text-changes {:name {:before "舊名" :after "新名"} :intent {:before "舊意圖" :after "新意圖"}}
    :merged 1}
   {:changeset-ids [5] :at (ts 11) :label "再改名" :by :agent :kind :change :undone? true
    :fields #{:name} :text-changes {:name {:before "新名" :after "被復原的名稱"}} :merged 1}
   {:changeset-ids [6] :at (ts 12) :label "復原：再改名" :by :user :kind :undo
    :fields #{:name} :text-changes {:name {:before "被復原的名稱" :after "新名"}} :merged 1}])

(defn- history-state [& [more?]]
  (-> note-state
      (ui/set-notes [{:note/id "t1" :note/node "a1" :note/author :user :note/text "寫短一點" :note/todo true
                      :note/state :open :note/at (ts 1)}
                     {:note/id "t2" :note/node "a1" :note/author :user :note/text "補數據" :note/at (ts 20)}])
      (ui/set-history "a1" {:entries history-entries :more? (boolean more?)})))

(deftest history-timeline-test
  (let [html (render (history-state))
        idx #(str/index-of html %)]
    (testing "留言串與修改依時間穿插，最後是新增留言"
      (is (< (idx "建立卡片") (idx "寫短一點") (idx "編輯文字") (idx "新名") (idx "補數據") (idx "compose-a1"))))
    (testing "事件列：改了什麼的欄位標籤、連續輸入、有本文時不列狀態"
      (let [fields-of #(vec (for [e (tree-seq coll? seq (views/app (history-state)))
                                  :when (and (vector? e) (= :span.tl-field (first e)))]
                              (last e)))]
        (is (= ["本文" "名稱" "意圖"] (sort-by {"本文" 0 "名稱" 1 "意圖" 2} (distinct (fields-of)))))
        (is (not (some #{"狀態"} (fields-of)))))
      (is (str/includes? html "連續輸入 2 次")))
    (testing "label 空字串時依 kind 顯示"
      (is (str/includes? html "[:span.tl-label \"修改\"]")))
    (testing "同一事件內：時間在操作者之前，其後是修改名稱"
      (let [ev (pr-str (find-el :div.tl-line (views/app (history-state))))]
        (is (< (str/index-of ev "tl-time") (str/index-of ev "tl-by") (str/index-of ev "tl-label")))))
    (testing "agent 與使用者的操作者與圖示樣式"
      (is (str/includes? html "tl-event-agent"))
      (is (str/includes? html "[:span.tl-by {:class \"tl-by-agent\"} \"agent\"]"))
      (is (str/includes? html "[:span.tl-by {:class \"tl-by-user\"} \"使用者\"]"))
      (is (str/includes? html "tl-agent")))
    (testing "名稱與意圖的舊新文字，舊的刪除線"
      (is (str/includes? html "[:div.tl-diff-name \"名稱\"]"))
      (is (str/includes? html "[:div.tl-diff-name \"意圖\"]"))
      (is (str/includes? html "[:del.tl-diff-text \"舊名\"]"))
      (is (str/includes? html "[:span.tl-diff-text \"新意圖\"]"))
      (is (< (str/index-of html "舊名") (str/index-of html "新名"))))
    (testing "已復原"
      (is (str/includes? html "tl-undone"))
      (is (str/includes? html "[:span.tl-undone-tag \"已復原\"]")))
    (testing "已復原的修改與復原本身只留一行，不列前後文字"
      (is (str/includes? html "復原：再改名"))
      (is (not (str/includes? html "被復原的名稱"))))
    (testing "只有本文有變動的項目有「看差異」，帶代表 id"
      (is (str/includes? html "[:ui/show-change \"a1\" 3]"))
      (is (not (str/includes? html "[:ui/show-change \"a1\" 1]")))
      (is (not (str/includes? html "[:ui/show-change \"a1\" 4]"))))
    (testing "區塊標題"
      (is (str/includes? html "留言與歷程"))
      (is (not (str/includes? html "留言與待辦"))))))

(deftest history-more-test
  (is (not (str/includes? (render (history-state)) "顯示較早的紀錄")))
  (let [html (render (history-state true))]
    (is (str/includes? html "顯示較早的紀錄"))
    (is (str/includes? html "[:history/show-all]"))
    (is (< (str/index-of html "顯示較早的紀錄") (str/index-of html "建立卡片")))))

(deftest history-other-card-test
  (testing "歷程是別張卡片的就不顯示（回應慢到時）"
    (let [s (-> (history-state) (assoc-in [:ui :history :id] "a2"))]
      (is (not (str/includes? (render s) "建立卡片"))))))

(deftest change-view-test
  (let [doc #(hash-map :type "doc" :content [{:type "paragraph" :content [{:type "text" :text %}]}])
        s (-> (history-state) (ui/show-change "a1" 3)
              (ui/set-change "a1" 3 {:prose? true :before (doc "舊文") :after (doc "新文") :at (ts 2.5) :label "編輯文字" :by :agent}))
        html (render s)]
    (testing "顯示那次修改的差異與說明"
      (is (str/includes? html ":del"))
      (is (str/includes? html ":ins"))
      (is (str/includes? html "編輯文字"))
      (is (str/includes? html "的修改（agent 修改）")))
    (testing "回到目前內容；切換兩者都不是 pressed"
      (is (str/includes? html "回到目前內容"))
      (is (str/includes? html "[:ui/show-current]"))
      (let [group (pr-str (find-el :div.segmented.segmented-small (views/app s)))]
        (is (= 2 (count (re-seq #":aria-pressed \"false\"" group))))
        (is (not (str/includes? group ":aria-pressed \"true\"")))))
    (testing "讀取中"
      (is (str/includes? (render (ui/show-change (history-state) "a1" 3)) "讀取修改…")))
    (testing "使用者修改不標 agent"
      (is (not (str/includes? (render (ui/set-change (ui/show-change (history-state) "a1" 3) "a1" 3
                                                     {:prose? true :before (doc "a") :after (doc "b") :by :user :label "x"}))
                              "agent 修改"))))
    (testing "那次修改沒有改本文（例如待辦連到只改意圖的修改）"
      (let [html (render (ui/set-change (ui/show-change (history-state) "a1" 3) "a1" 3
                                        {:prose? false :by :agent :label "改意圖"}))]
        (is (str/includes? html "這次修改沒有改動本文"))
        (is (not (str/includes? html "diff-legend")))))))

(def mixed-state
  (ui/init-state
   {:report/title "測試報告"
    :ui/selected "H"
    :tree [{:node/id "H" :node/kind :heading :node/name "甲"
            :children [{:node/id "i" :node/kind :intro :node/draft "概述"}
                       {:node/id "p1" :node/kind :paragraph :node/title "章段落"}
                       {:node/id "S" :node/kind :heading :node/name "乙"
                        :children [{:node/id "s1" :node/kind :paragraph :node/title "節段落"}]}
                       {:node/id "T" :node/kind :heading :node/name "丙"}]}]}))

(deftest heading-structure-buttons-test
  (testing "已有段落的標題：可以同時新增段落、新增下層標題，並保留「移入段落」"
    (let [html (render (ui/open-editor mixed-state))]
      (is (str/includes? html "[:ui/add-child \"H\" :paragraph]"))
      (is (str/includes? html "[:ui/add-child \"H\" :heading]"))
      (is (str/includes? html "[:ui/wrap-paragraphs \"H\"]"))
      (is (str/includes? html "段落、下層標題"))
      (is (not (str/includes? html "不混用")))))
  (testing "沒有段落的標題：不提供「移入段落」"
    (let [html (render (ui/open-editor (ui/select mixed-state "T")))]
      (is (str/includes? html "[:ui/add-child \"T\" :paragraph]"))
      (is (not (str/includes? html "wrap-paragraphs"))))))

(deftest mixed-fulltext-order-test
  (let [html (pr-str (find-el :div.fulltext-page (views/app (ui/set-page mixed-state :fulltext))))
        idx #(str/index-of html %)]
    (is (< (idx "第一章 甲") (idx "章段落") (idx "1.1 乙") (idx "節段落"))
        "全文順序：標題、段落、下層標題、下層的段落")))

(deftest mixed-tree-columns-test
  (let [html (render mixed-state)]
    (is (str/includes? html "節／段落") "段落與下層標題同一欄時，欄名同時列出")))
