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

(deftest agent-change-test
  (let [s (ui/agent-changed state {:label "建立大綱：3 張卡片" :nodes ["a2"]})
        html (render s)]
    (testing "agent 修改的卡片標示，再次修改時換成另一個動畫"
      (is (str/includes? html "card-agent-b"))
      (is (str/includes? (render (ui/agent-changed s {:label "x" :nodes ["a2"]})) "card-agent-a")))
    (is (not (str/includes? (render state) "card-agent")) "沒有 agent 修改時不標示")))

(deftest recent-menu-test
  (testing "卡片工具列有「最近修改」，清單預設關閉"
    (let [html (render state)]
      (is (str/includes? html "最近修改 ▾"))
      (is (not (str/includes? html "recent-menu")))))
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
    (testing "留言與待辦是編輯區最後一個區塊，最新的在下方"
      (is (< (idx "與前一版比較") (idx "留言與待辦")))
      (is (< (idx "寫短一點") (idx "好的") (idx "補數據"))))
    (testing "待辦有勾選框；完成的待辦可以看修改"
      (is (str/includes? html "[:notes/set-state \"t1\" :done]"))
      (is (str/includes? html "[:notes/set-state \"t2\" :open]"))
      (is (str/includes? html "[:ui/open-change \"a1\"]")))
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
