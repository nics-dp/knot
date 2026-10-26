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
    (testing "段落編輯區依序是生成依據、AI 會參考的內容、本文"
      (let [idx #(str/index-of html %)]
        (is (< (idx "生成依據") (idx "AI 會參考的內容") (idx "與前一版比較")))))
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
