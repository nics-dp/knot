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
    (testing "全文頁沒有看板工具列，編輯區的「⋯」選單可以刪除選取的卡片"
      (is (str/includes? html "more-menu"))
      (is (str/includes? html (str "[:ui/delete-node \"" (get-in state [:ui :selected]) "\"]"))))
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

(deftest fulltext-intro-test
  (let [s (ui/init-state
           {:report/title "測試報告"
            :tree [{:node/id "H" :node/kind :heading :node/name "甲"
                    :children [{:node/id "i" :node/kind :intro
                                :node/prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "本章說明"}]}]}}
                               {:node/id "p" :node/kind :paragraph :node/title "背景"
                                :node/prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "段落本文"}]}]}}]}]})
        intro (find-el :div.ft-block.ft-intro (views/app (ui/set-page s :fulltext)))]
    (testing "全文頁的引言有「引言」小標籤，段落沒有"
      (is (str/includes? (pr-str intro) "[:div.ft-kind \"引言\"]"))
      (is (= 1 (count (find-all :div.ft-kind (views/app (ui/set-page s :fulltext)))))))))

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
      (is (str/includes? html "1 則待辦事項交給 agent")))))

(deftest todos-menu-test
  (is (str/includes? (render note-state) "[:span.count-badge 2]") "未完成的待辦與 agent 的提問")
  (let [html (render (ui/toggle-todos note-state))]
    (is (str/includes? html "交給 agent（1）"))
    (is (str/includes? html "[:ui/open-note-thread \"a2\" \"q1\"]") "agent 的提問直接打開回覆框")
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
  (testing "已有段落的標題：可以同時新增段落、新增下層標題，不提供「移入段落」"
    (let [html (render (ui/open-editor mixed-state))]
      (is (str/includes? html "[:ui/add-child \"H\" :paragraph]"))
      (is (str/includes? html "[:ui/add-child \"H\" :heading]"))
      (is (not (str/includes? html "wrap-paragraphs")))
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

;; ---------------------------------------------------------------------------
;; M6：清單、表格、圖、未綁定的數字

(def list-doc
  {:type "doc"
   :content [{:type "paragraph" :content [{:type "text" :text "主要成果如下："}]}
             {:type "bulletList"
              :content [{:type "listItem" :content [{:type "paragraph" :content [{:type "text" :text "甲"}]}]}
                        {:type "listItem" :content [{:type "paragraph"
                                                     :content [{:type "text" :text "乙"}
                                                               {:type "cite" :attrs {:block "b1"}}]}]}]}
             {:type "orderedList"
              :content [{:type "listItem" :content [{:type "paragraph" :content [{:type "text" :text "丙"}]}]}]}]})

(deftest prose-view-lists-test
  (let [v (vec (views/prose-view {} list-doc [{:block/id "b1" :block/title "維運報告"}]))]
    (is (= 3 (count v)))
    (is (= :p.prose (first (v 0))))
    (is (= :ul.prose-list (first (v 1))))
    (is (= :ol.prose-list (first (v 2))))
    (is (= 2 (count (find-all :li (v 1)))))
    (testing "cite 顯示區塊標題，title 帶完整來源"
      (let [chip (find-el :span.chip.chip-cite (v 1))]
        (is (= "維運報告" (last chip)))
        (is (str/includes? (:title (second chip)) "b1"))))
    (testing "找不到區塊時顯示「來源」"
      (is (= "來源" (last (find-el :span.chip.chip-cite (views/prose-view {} list-doc []))))))))

(def float-state
  (ui/init-state
   {:report/title "測試報告"
    :ui/selected "t1"
    :facts [{:fact/key "k" :fact/label "某事實" :fact/value "1" :fact/unit "" :fact/block "b1"}]
    :tree [{:node/id "A" :node/kind :heading :node/name "甲"
            :children [{:node/id "a1" :node/kind :paragraph :node/title "一"
                        :node/prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "共 2,871 人次，達成 95.7%，另 114 件"}]}]}
                        :children [{:node/id "t1" :node/kind :table :node/title "達成總表" :node/label "tab:sum"
                                    :node/table-content {:columns ["指標" "數值"] :rows [["人次" "2,871"] ["達成率" "95.7%"]]}}
                                   {:node/id "t2" :node/kind :table :node/title "空表" :node/label "tab:empty"}
                                   {:node/id "f1" :node/kind :figure :node/title "架構圖" :node/label "fig:arch"}]}]}]}))

(defn- open-editor [state id]
  (-> state (ui/select id) ui/open-editor))

(deftest table-editor-test
  (let [v (views/app (open-editor float-state "t1"))]
    (is (str/includes? (pr-str v) "表格內容"))
    (is (str/includes? (pr-str v) "write_table"))
    (is (= 2 (count (find-all :th v))))
    (is (= 4 (count (find-all :td v)))))
  (let [s (pr-str (views/app (open-editor float-state "t2")))]
    (is (str/includes? s "尚未填寫：在下方"))
    (is (not (str/includes? s ":table.data-table"))))
  (testing "圖有虛線佔位"
    (is (str/includes? (pr-str (views/app (open-editor float-state "f1"))) "images/arch.tex，圖檔請在 LaTeX 中放入"))))

(deftest paragraph-editor-floats-and-unbound-test
  (let [s (pr-str (views/app (open-editor float-state "a1")))]
    (testing "下層表與圖有新增按鈕，action 帶段落 id 與類型"
      (is (str/includes? s "新增表格"))
      (is (str/includes? s "[:ui/add-child \"a1\" :table]"))
      (is (str/includes? s "[:ui/add-child \"a1\" :figure]")))
    (testing "未綁定的數字以 chip 列出"
      (is (str/includes? s "未綁定的數字"))
      (is (every? #(str/includes? s %) ["2,871" "95.7%" "114"]))))
  (testing "沒有下層表與圖時欄位仍顯示說明"
    (let [s (pr-str (views/app (open-editor state "a2")))]
      (is (str/includes? s "新增表格"))
      (is (str/includes? s "建立後在本文以 label 引用"))))
  (testing "報告沒有任何事實時不提醒"
    (is (not (str/includes? (pr-str (views/app (open-editor (assoc float-state :facts {}) "a1"))) "未綁定的數字"))))
  (testing "年月日與章節編號不列入"
    (let [s (-> float-state
                (assoc-in [:nodes "a1" :node/prose]
                          {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "114 年 3 月 15 日，見第 2.1.1 節，共 7 站"}]}]})
                (open-editor "a1") views/app pr-str)]
      (is (str/includes? s "未綁定的數字"))
      (is (not (str/includes? s "2.1.1")))
      (is (not (str/includes? s "\"114\"")))))
  (testing "沒有裸數字時不顯示提醒"
    (is (not (str/includes? (pr-str (views/app (open-editor state "a1"))) "未綁定的數字"))))
  (testing "超過 12 個以「等 N 個」表示"
    (let [many (assoc-in float-state [:nodes "a1" :node/prose]
                         {:type "doc" :content [{:type "paragraph"
                                                 :content [{:type "text" :text (str/join " " (range 100 115))}]}]})
          s (pr-str (views/app (open-editor many "a1")))]
      (is (str/includes? s "等 15 個")))))

(deftest fulltext-floats-test
  (let [v (views/app (ui/set-page (ui/select float-state "A") :fulltext))
        s (pr-str v)]
    (is (str/includes? s "表 tab:sum"))
    (is (str/includes? s "圖 fig:arch"))
    (is (= 1 (count (find-all :table.data-table v))) "有內容的表格顯示成表格")
    (is (str/includes? s "（尚未填寫）"))
    (is (str/includes? s "images/arch.tex，圖檔請在 LaTeX 中放入"))))

(deftest table-card-unfilled-badge-test
  (is (str/includes? (pr-str (views/app float-state)) "尚未填寫")))

(deftest count-info-test
  (testing "沒有字數的卡片或沒有內容也沒目標：不顯示"
    (is (nil? (views/count-info nil nil)))
    (is (nil? (views/count-info nil 400)))
    (is (nil? (views/count-info 0 nil))))
  (testing "只有字數"
    (is (= {:text "312 字"} (views/count-info 312 nil)))
    (is (= {:text "12,345 字"} (views/count-info 12345 nil))))
  (testing "沒有本文但有目標"
    (is (= "目標 400 字" (:text (views/count-info 0 400)))))
  (testing "字數／目標；80%–120% 之間不上色"
    (is (= {:text "330／400 字"} (views/count-info 330 400)))
    (is (nil? (:class (views/count-info 320 400))) "剛好 80%")
    (is (nil? (:class (views/count-info 480 400))) "剛好 120%"))
  (testing "低於 80% 為不足、超過 120% 為超出，title 說明差多少"
    (let [low (views/count-info 260 400) high (views/count-info 500 400)]
      (is (= "count-low" (:class low)))
      (is (= "比目標少 35%（目標 400 字）" (:title low)))
      (is (= "count-high" (:class high)))
      (is (= "比目標多 25%（目標 400 字）" (:title high))))))

(def count-state
  (ui/init-state
   {:report/title "測試報告"
    :ui/selected "p1"
    :tree [{:node/id "H" :node/kind :heading :node/name "甲" :node/target-chars 1000
            :children [{:node/id "p1" :node/kind :paragraph :node/title "背景" :node/target-chars 10
                        :node/prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "一二 三四五六七八九十十一"}]}]}}
                       {:node/id "p2" :node/kind :paragraph :node/title "目標" :node/target-chars 400}
                       {:node/id "p3" :node/kind :paragraph :node/title "已寫"
                        :node/prose {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "已有本文"}]}]}}]}]}))

(defn- spans-with
  "帶有指定 class 的 :span（class 在屬性的 :class 向量裡）的文字。"
  [cls hiccup]
  (vec (for [el (tree-seq coll? seq hiccup)
             :when (and (vector? el) (= :span (first el)) (map? (second el))
                        (some #{cls} (:class (second el))))]
         (last el))))

(deftest count-views-test
  (testing "看板卡片：段落的字數／目標、標題合計、沒有本文只顯示目標"
    (let [shown (set (spans-with "card-count" (views/app count-state)))]
      (is (contains? shown "12／10 字") (pr-str shown))
      (is (contains? shown "目標 400 字"))
      (is (contains? shown "4 字"))
      (is (contains? shown "16／1,000 字") "標題合計（12 + 4）對 1,000 字目標")))
  (testing "全文頁的大綱不顯示字數"
    (let [view (views/app (ui/set-page count-state :fulltext))]
      (is (nil? (find-el :div.outline-total view)))
      (is (empty? (spans-with "outline-count" view)))))
  (testing "編輯區：本文區的字數、目標字數欄位、標題的合計"
    (let [view (views/app (assoc-in count-state [:ui :editor-collapsed] false))
          html (pr-str view)]
      (is (= ["12／10 字"] (spans-with "zone-count" view)))
      (is (str/includes? html "目標字數"))
      (is (str/includes? html "[:ui/set-target-chars \"p1\" [:event/target-value]]"))))
  (testing "標題的編輯區顯示整章合計"
    (let [view (views/app (-> count-state (assoc-in [:ui :editor-collapsed] false) (assoc-in [:ui :selected] "H")))]
      (is (= ["整章合計 16／1,000 字"] (spans-with "target-total" view))))))

;; ---------------------------------------------------------------------------
;; 素材庫

(def lib-state
  (-> (ui/init-state
       {:report/title "測試報告"
        :ui/selected "a1"
        :tree [{:node/id "A" :node/kind :heading :node/name "甲"
                :children [{:node/id "a1" :node/kind :paragraph :node/title "一"
                            :node/citations [{:block "b1"} {:block "b3"}]}]}]
        :blocks [{:block/id "b1" :block/kind :para :block/status :confirmed :block/title "節電成果"
                  :block/source "期中報告.pdf" :block/location "p.3" :block/note "引用節電率"
                  :block/text "全年節電率達 12.5%。"}
                 {:block/id "b2" :block/kind :table :block/status :confirmed :block/title "站點人次"
                  :block/source "統計.xlsx" :block/location "工作表 1"
                  :table/columns ["站點" "人次"] :table/rows [["甲站" "1,200"] ["乙站" "800"]]}
                 {:block/id "b3" :block/kind :para :block/status :discarded :block/title "舊說明"
                  :block/source "期中報告.pdf" :block/text "已廢止的節電說明。"}]
        :facts [{:fact/key "saving.rate" :fact/label "節電率" :fact/value "12.5" :fact/unit "%" :fact/block "b1"}
                {:fact/key "ridership.b" :fact/label "乙站人次" :fact/value "800" :fact/block "b2"
                 :fact/row 1 :fact/column "人次"}]})
      (ui/set-page :library)))

(defn- block-ids [blocks] (mapv :block/id blocks))

(deftest filter-blocks-test
  (let [bs (:blocks lib-state)]
    (is (= ["b1" "b2"] (block-ids (views/filter-blocks bs :all ""))) "預設排除已捨棄")
    (is (= ["b1" "b2" "b3"] (sort (block-ids (views/filter-blocks bs :all "" {:include-discarded? true})))))
    (is (= ["b2"] (block-ids (views/filter-blocks bs :table ""))))
    (is (= ["b1"] (block-ids (views/filter-blocks bs :para ""))))
    (is (= ["b1"] (block-ids (views/filter-blocks bs :all "節電 期中"))) "多詞")
    (is (= ["b2"] (block-ids (views/filter-blocks bs :all "ＸＬＳＸ 甲站"))) "全形與多詞（NFKC）")
    (is (= ["b1"] (block-ids (views/filter-blocks bs :all "" {:source "期中報告.pdf"}))))))

(deftest fact-cell-text-test
  (is (= "第 2 列・人次" (views/fact-cell-text {:fact/row 1 :fact/column "人次"})))
  (is (nil? (views/fact-cell-text {:fact/key "x"}))))

(deftest library-list-test
  (let [html (render lib-state)]
    (testing "類型標籤用「節錄」，沒有候選／確認"
      (is (str/includes? html "節錄"))
      (is (not (str/includes? html "未確認")))
      (is (not (str/includes? html "AI 自動切出")))
      (is (not (str/includes? html "已確認"))))
    (testing "已捨棄的預設不列出；來源檔顯示素材數（不含已捨棄）"
      (is (not (str/includes? html "舊說明")))
      (is (str/includes? html "1 筆")))
    (testing "事實數與被引用數"
      (is (str/includes? html "1 個事實"))
      (is (str/includes? html "被 1 個段落引用"))))
  (testing "顯示已捨棄：淡化並標「已捨棄」"
    (let [html (render (ui/toggle-show-discarded lib-state))]
      (is (str/includes? html "舊說明"))
      (is (str/includes? html "block-discarded"))
      (is (str/includes? html "已捨棄"))))
  (testing "搜尋與類型篩選"
    (let [html (render (-> lib-state (ui/set-source-query "人次") (ui/set-source-filter :table)))]
      (is (str/includes? html "站點人次"))
      (is (not (str/includes? html "節電成果")))))
  (testing "沒有素材時的說明"
    (is (str/includes? (render (assoc lib-state :blocks [])) views/empty-library-text))))

(deftest library-detail-test
  (testing "節錄詳情：用途說明、原文、事實、引用它的段落、捨棄"
    (let [html (render (ui/select-block lib-state "b1"))]
      (is (str/includes? html "用途說明"))
      (is (str/includes? html "引用節電率"))
      (is (str/includes? html "全年節電率達 12.5%。"))
      (is (str/includes? html "saving.rate"))
      (is (str/includes? html "引用它的段落"))
      (is (str/includes? html "[:ui/open-paragraph \"a1\"]"))
      (is (str/includes? html "[:ui/set-block-status \"b1\" :discarded]"))))
  (testing "表格詳情：事實指向的儲存格標出，並說明第 N 列・欄名"
    (let [v (views/app (ui/select-block lib-state "b2"))
          marked (filter #(= "cell-mark" (:class (second %))) (find-all :span v))]
      (is (= ["800"] (map #(nth % 2) marked)))
      (is (str/includes? (pr-str v) "第 2 列・人次"))))
  (testing "已捨棄的素材：顯示「恢復」"
    (let [html (render (-> lib-state ui/toggle-show-discarded (ui/select-block "b3")))]
      (is (str/includes? html "[:ui/set-block-status \"b3\" :confirmed]"))
      (is (str/includes? html "恢復")))))

(deftest picker-test
  (let [open (-> lib-state (ui/set-page :tree) (ui/select "a1") ui/open-editor ui/open-picker)
        picker #(pr-str (find-el :div.picker (views/app %)))]
    (is (str/includes? (picker open) "站點人次"))
    (is (not (str/includes? (picker open) "舊說明")) "選取器排除已捨棄")
    (let [p (picker (ui/set-picker-query open "站點"))]
      (is (str/includes? p "站點人次"))
      (is (not (str/includes? p "節電成果"))))))

(deftest citation-discarded-test
  (let [html (render (-> lib-state (ui/set-page :tree) (ui/select "a1") ui/open-editor))]
    (is (str/includes? html "已捨棄"))
    (is (not (str/includes? html "未確認素材")))))

(deftest citation-jump-to-library-test
  (testing "編輯區依據列的名稱帶 :ui/open-block"
    (let [s (-> lib-state (ui/set-page :tree) (assoc-in [:ui :editor-collapsed] false) (assoc-in [:ui :selected] "a1"))
          html (render s)]
      (is (str/includes? html "[:ui/open-block \"b1\"]"))
      (is (str/includes? html "[:ui/open-block \"b3\"]") "已捨棄的也能跳")))
  (testing "跳到素材庫後詳情顯示該素材（已捨棄的清單也列出）"
    (let [s (ui/open-block (ui/set-page lib-state :tree) "b3")
          html (render s)]
      (is (= :library (get-in s [:ui :page])))
      (is (str/includes? html "已廢止的節電說明"))
      (is (str/includes? html "[:ui/set-block-status \"b3\" :confirmed]")))))

(deftest fact-chip-display-test
  (let [s (-> lib-state (assoc-in [:facts "n"] {:fact/key "n" :fact/label "人次" :fact/value "3760" :fact/unit "人次" :fact/block "b1"}))
        html (render (ui/select-block s "b1"))]
    (is (not (str/includes? (render (ui/select-block lib-state "b1")) "3,760")))
    (is (str/includes? html "3,760 人次") "素材庫的事實清單")))

(deftest style-tab-and-page-test
  (testing "頂部分頁：風格在全文之後"
    (is (= [[:library "素材"] [:tree "卡片"] [:fulltext "全文"] [:style "風格"]] views/tabs))
    (is (str/includes? (render state) "風格")))
  (let [s (ui/set-page state :style)
        html (render s)]
    (testing "風格頁：標題、說明、字數、編輯框，沒有看板與編輯區"
      (is (find-el :section.style-page (views/app s)))
      (is (str/includes? html "agent 撰寫每一段時都會依這份風格"))
      (is (str/includes? html "0 字"))
      (is (str/includes? html "用語與語氣"))
      (is (not (str/includes? html "tree-panel"))))
    (testing "文字框綁 set-style-text"
      (let [ta (find-el :textarea.intent.style-input (views/app s))]
        (is (= [[:ui/set-style-text [:event/target-value]]] (get-in (second ta) [:on :input])))
        (is (= "" (:value (second ta))))))
    (testing "有風格時顯示內容、字數、最後修改者"
      (let [s (-> s (ui/set-style-text "使用「本計畫」") (assoc :style-info {:updated-by :agent :updated-at nil}))
            html (render s)]
        (is (= "使用「本計畫」" (:value (second (find-el :textarea.intent.style-input (views/app s))))))
        (is (str/includes? html "7 字 · 最後修改：agent"))))))

(deftest context-style-row-test
  (testing "編輯區的撰寫參考不列風格（在風格頁檢視）"
    (let [s (ui/set-style-text (ui/select (ui/open-editor state) "a2") "使用「本計畫」，避免口語。")
          html (pr-str (views/app s))]
      (is (not (str/includes? html "使用「本計畫」")))
      (is (not (str/includes? html "context-style"))))))

(def knot-state
  (ui/set-notes (ui/open-editor state)
                [{:note/id "k1" :note/node "a1" :note/author :knot :note/todo true :note/state :open
                  :note/text "「甲 二」的本文改了，本段摘要它，可能需要跟著更新。"
                  :note/triggers [{:id "a2" :dependency :summary :reason "摘要它"} {:id "A" :dependency :argument :reason "因果承接"}]
                  :note/modified-after true :note/at 1}
                 {:note/id "q2" :note/node "a2" :note/author :agent :note/text "要寫幾年？" :note/at 2}
                 {:note/id "u1" :note/node "a2" :note/author :user :note/text "補數據" :note/todo true :note/state :open :note/at 3}]))

(deftest knot-todo-view-test
  (let [html (render knot-state)]
    (testing "作者標籤為 Knot，與 agent、使用者區分"
      (is (str/includes? html "by-knot"))
      (is (str/includes? html "\"Knot\""))
      (is (not (str/includes? html "by-agent"))))
    (testing "待辦有勾選框，觸發的卡片可以點選跳過去，使用者可以刪除"
      (is (str/includes? html "[:notes/set-state \"k1\" :done]"))
      (is (str/includes? html "[:ui/open-note-card \"a2\"]"))
      (is (str/includes? html "[:notes/delete \"k1\"]"))
      (testing "多個觸發卡片各有一個連結"
        (is (str/includes? html "trigger-a2"))
        (is (str/includes? html "trigger-A"))
        (is (str/includes? html "這些段落改了，本段摘要它們、因果承接它們，可能需要跟著更新：")
            "說明不重複段落名稱")
        (is (not (str/includes? html "「甲 二」的本文改了")) "不顯示留言原本列出名稱的文字")
        (is (< (str/index-of html "note-text") (str/index-of html "note-triggers"))
            "放在內容下方，不擠在標題列")
        (is (not (str/includes? html "看「")) "連結只寫卡片名稱"))
      (testing "之後已修改的標籤"
        (is (str/includes? html "之後已修改")))
      (is (not (str/includes? html "標為待辦事項")) "Knot 的待辦不能取消待辦"))
    (testing "卡片的待辦數"
      (is (str/includes? html "1 則待辦事項交給 agent"))))

  (testing "頂部列待辦清單列出 Knot 待辦"
    (let [html (render (ui/toggle-todos knot-state))]
      (is (str/includes? html "[:ui/open-note-card \"a1\"]"))
      (is (str/includes? html "Knot"))
      (testing "分兩組，組標題帶數量，按鈕數字為合計"
        (is (str/includes? html "等你回覆（1）"))
        (is (str/includes? html "交給 agent（2）"))
        (is (< (str/index-of html "等你回覆") (str/index-of html "交給 agent")))
        (is (str/includes? html "[:span.count-badge 3]"))))))

(deftest note-badges-test
  (let [html (render knot-state)]
    (testing "等你回覆的提問與交給 agent 的待辦事項分成兩個數字"
      (is (str/includes? html "question-count"))
      (is (str/includes? html "1 則 agent 的提問等你回覆"))
      (is (str/includes? html "1 則待辦事項交給 agent"))))
  (testing "全文大綱的標題是含下層的合計"
    (let [html (render (ui/set-page knot-state :fulltext))]
      (is (str/includes? html "1 則 agent 的提問等你回覆（含下層）"))
      (is (str/includes? html "2 則待辦事項交給 agent（含下層）")))))

(deftest report-setup-view-test
  (let [s (ui/init-state {:report/title "目錄名" :tree []})
        html (render s)]
    (testing "空白報告先顯示名稱表單，不顯示新增第一章與匯入範例"
      (is (str/includes? html "報告名稱"))
      (is (str/includes? html "副標（選填）"))
      (is (str/includes? html ":value \"目錄名\""))
      (is (str/includes? html "[:report/save-setup]"))
      (is (not (str/includes? html "新增第一章")))
      (is (not (str/includes? html "匯入範例內容"))))
    (testing "名稱空白時確定為 disabled"
      (is (str/includes? (render (ui/set-report-setup-draft s :title "")) ":disabled true")))
    (testing "確認後顯示原本的空白畫面；匯入範例只在開發模式"
      (let [named (assoc-in s [:ui :report-named] true)
            h (render named)]
        (is (str/includes? h "新增第一章"))
        (is (not (str/includes? h "匯入範例內容")))
        (is (str/includes? (render (assoc named :dev? true)) "匯入範例內容"))))))

;; 全文頁區塊的數字、標題的下層清單 ------------------------------------------------

(def badge-state
  (-> (ui/init-state
       {:report/title "測試報告"
        :tree [{:node/id "A" :node/kind :heading :node/name "甲"
                :children [{:node/id "a1" :node/kind :paragraph :node/title "一"}
                           {:node/id "a2" :node/kind :paragraph :node/title "二"}
                           {:node/id "f1" :node/kind :figure :node/title "流程圖" :node/label "fig:f"}]}]})
      (ui/set-notes [{:note/id "t1" :note/node "a1" :note/author :user :note/text "補數據\n第二行"
                      :note/todo true :note/state :open :note/at 1}
                     {:note/id "q1" :note/node "a1" :note/author :agent :note/text "要寫幾年？" :note/at 2}
                     {:note/id "t2" :note/node "f1" :note/author :user :note/text "換圖" :note/todo true
                      :note/state :open :note/at 3}
                     {:note/id "q0" :note/node "A" :note/author :agent :note/text "標題自己的提問" :note/at 4}])))

(deftest fulltext-block-badges-test
  (let [v (views/app (ui/set-page badge-state :fulltext))
        blocks (find-all :div.ft-badges v)
        html (pr-str v)]
    (is (= 2 (count blocks)) "只有自己有數字的段落與圖，不含下層合計、也沒有數字的 a2 不顯示")
    (is (str/includes? html "[:ui/open-note-thread \"a1\" \"q1\"]") "提問的數字直接打開回覆框")
    (is (str/includes? html "[:ui/open-note-thread \"a1\"]") "待辦的數字只開啟留言")
    (is (str/includes? html "[:ui/open-note-thread \"f1\"]"))
    (is (not (str/includes? html "[:ui/open-note-thread \"a2\"]")))
    (is (str/includes? html "ft-noted"))))

(deftest heading-descendant-list-test
  (let [html (render (ui/open-editor (ui/select badge-state "A")))
        idx #(str/index-of html %)]
    (is (str/includes? html "下層的待辦事項與提問（3）"))
    (is (< (idx "下層的待辦事項與提問") (idx "tl-item")) "清單在時間軸之前")
    (is (< (idx "d-q1") (idx "d-t1") (idx "d-t2")) "提問在前")
    (is (str/includes? html "第一章 一"))
    (is (str/includes? html "流程圖"))
    (is (str/includes? html "補數據"))
    (is (not (str/includes? html "第二行")) "只取第一行")
    (is (str/includes? html "[:ui/open-note-thread \"a1\" \"q1\"]") "提問直接打開回覆框")
    (is (str/includes? html "[:ui/open-note-thread \"a1\"]") "待辦事項只開啟留言"))
  (testing "段落的編輯區沒有這個清單"
    (is (not (str/includes? (render (ui/open-editor (ui/select badge-state "a1")))
                            "下層的待辦事項與提問")))))

(def appendix-state
  (ui/init-state
   {:report/title "測試報告"
    :ui/selected "A"
    :tree [{:node/id "A" :node/kind :heading :node/name "甲"
            :children [{:node/id "a1" :node/kind :paragraph :node/title "一"}
                       {:node/id "A1" :node/kind :heading :node/name "甲一"}]}
           {:node/id "X" :node/kind :heading :node/name "逐週數據" :node/appendix true
            :children [{:node/id "X1" :node/kind :heading :node/name "週報"}]}]}))

(deftest appendix-menu-test
  (let [menu-actions (fn [s]
                       (->> (find-all :button.menu-item (views/app (ui/open-editor s)))
                            (map (juxt #(last %) #(get-in (second %) [:on :click])))))]
    (testing "一般章的「⋯」選單有「設為附錄」"
      (is (some #{["設為附錄" [[:ui/set-appendix "A" true]]]} (menu-actions appendix-state))))
    (testing "附錄章有「取消附錄」"
      (let [s (assoc-in appendix-state [:ui :selected] "X")]
        (is (some #{["取消附錄" [[:ui/set-appendix "X" false]]]} (menu-actions s)))))
    (testing "下層標題與段落沒有"
      (doseq [id ["A1" "a1" "X1"]]
        (is (not-any? #(#{"設為附錄" "取消附錄"} (first %))
                      (menu-actions (assoc-in appendix-state [:ui :selected] id))))))))

(deftest appendix-numbering-display-test
  (let [html (render (ui/set-page appendix-state :fulltext))]
    (testing "全文頁的標題與大綱用附錄編號"
      (is (str/includes? html "附錄 A 逐週數據"))
      (is (str/includes? html "A.1 週報"))
      (is (str/includes? html "第一章 甲"))))
  (testing "編輯區的名稱框前綴"
    (let [html (render (ui/open-editor (assoc-in appendix-state [:ui :selected] "X")))]
      (is (str/includes? html "附錄 A")))))
