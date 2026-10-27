(ns knot.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [knot.tree :as tree]
            [knot.ui :as ui]))

(def report
  {:report/title "測試報告"
   :ui/selected "a1"
   :tree [{:node/id "A" :node/kind :heading :node/name "甲"
           :children [{:node/id "a1" :node/kind :paragraph :node/title "一" :node/status :ai}
                      {:node/id "a2" :node/kind :paragraph :node/title "二" :node/status :human}]}
          {:node/id "B" :node/kind :heading :node/name "乙"
           :children [{:node/id "b1" :node/kind :paragraph :node/title "三" :node/status :none}]}]
   :relations [{:relation/from "a1" :relation/to "a2" :relation/type :causal}]})

(def state (ui/init-state report))

(deftest navigate-test
  (is (= "a2" (get-in (ui/navigate state :down) [:ui :selected])))
  (is (= "A" (get-in (ui/navigate state :left) [:ui :selected])))
  (is (= "a1" (get-in (ui/navigate state :up) [:ui :selected])) "沒有上一個兄弟時不動"))

(deftest drag-and-drop-test
  (let [s (-> state
              (ui/drag-start "a1")
              ;; 欄 0 的 B 卡片在 92–176
              (ui/drag-over 0 120))]
    (is (= {:id "B" :pos :into} (get-in s [:ui :over])))
    (let [s (ui/drop-dragged s)]
      (testing "移動後被移動的卡片成為選取狀態，並顯示訊息"
        (is (= "a1" (get-in s [:ui :selected])))
        (is (= ["b1" "a1"] (tree/child-ids (:nodes s) "B")))
        (is (re-find #"待確認" (get-in s [:ui :toast]))))
      (testing "拖曳狀態清除"
        (is (nil? (get-in s [:ui :drag]))))
      (testing "整個移動可以復原"
        (let [u (ui/undo s)]
          (is (= (:nodes state) (:nodes u)))
          (is (= (:relations state) (:relations u)))
          (is (empty? (:history u))))))))

(deftest illegal-drop-test
  (let [s (-> state (ui/drag-start "a1") (ui/drag-over 3 0))]
    (is (nil? (get-in s [:ui :over])))
    (is (= (:nodes state) (:nodes (ui/drop-dragged s))))))

(deftest move-selected-test
  (let [s (ui/move-selected (ui/select state "a2") -1)]
    (is (= ["a2" "a1"] (tree/child-ids (:nodes s) "A")))
    (is (:relation/pending (tree/relation-between (:relations s) "a2" "a1")))))

(deftest add-child-test
  (testing "新增引言放在最前面"
    (let [s (ui/add-child state "A" :intro)
          id (get-in s [:ui :selected])]
      (is (= :intro (tree/kind (:nodes s) id)))
      (is (= id (first (tree/child-ids (:nodes s) "A"))))
      (testing "引言最多一張"
        (is (= (:nodes s) (:nodes (ui/add-child s "A" :intro)))))))
  (testing "已有段落的標題不能直接新增下層標題"
    (is (= (:nodes state) (:nodes (ui/add-child state "A" :heading)))))
  (testing "把現有段落移到新的下層標題"
    (let [s (ui/wrap-paragraphs state "A")
          sub (get-in s [:ui :selected])]
      (is (= [sub] (tree/child-ids (:nodes s) "A")))
      (is (= ["a1" "a2"] (tree/child-ids (:nodes s) sub))))))

(deftest paragraph->intro-test
  (let [s (ui/paragraph->intro state "a1")]
    (is (= :intro (tree/kind (:nodes s) "a1")))
    (is (= ["a1" "a2"] (tree/child-ids (:nodes s) "A")))
    (is (empty? (:relations s)) "引言不再有段落關係")))

(deftest toggle-lock-test
  (let [s (ui/toggle-lock state "a2")]
    (is (= :locked (get-in s [:nodes "a2" :node/status])))
    (is (= :human (get-in (ui/toggle-lock s "a2") [:nodes "a2" :node/status])))))

(deftest edit-intent-test
  (let [s (ui/set-node-field state "a1" :node/draft "新的意圖")]
    (is (= "新的意圖" (get-in s [:nodes "a1" :node/draft])))
    (is (empty? (:history s)) "逐字輸入不進復原紀錄"))
  (is (= state (ui/set-node-field state "a1" :node/status :locked)) "只能編輯名稱與意圖"))

(deftest citation-test
  (let [s (ui/add-citation state "a1" "b9")]
    (is (= [{:block "b9"}] (get-in s [:nodes "a1" :node/citations])))
    (is (= s (ui/add-citation s "a1" "b9")) "不重複加入")
    (is (empty? (get-in (ui/remove-citation s "a1" "b9") [:nodes "a1" :node/citations]))))
  (is (= state (ui/add-citation state "A" "b9")) "只有段落有依據"))

(deftest set-relation-test
  (let [moved (ui/move-selected (ui/select state "a2") -1)
        s (ui/set-relation moved "a2" "a1" :contrast)
        rel (tree/relation-between (:relations s) "a2" "a1")]
    (is (= :contrast (:relation/type rel)))
    (is (false? (:relation/pending rel)) "設定類型即確認")
    (is (nil? (tree/relation-between (:relations (ui/set-relation s "a2" "a1" nil)) "a2" "a1"))
        "選「未標註」移除關係")))

(deftest pages-test
  (let [s (-> state (ui/set-page :library) (ui/open-paragraph "b1"))]
    (is (= :tree (get-in s [:ui :page])))
    (is (= "b1" (get-in s [:ui :selected])))))

(deftest add-sibling-test
  (let [s (ui/add-sibling state "a1")
        id (get-in s [:ui :selected])]
    (testing "新段落接在選取段落的後面，並準備編輯名稱"
      (is (= ["a1" id "a2"] (tree/child-ids (:nodes s) "A")))
      (is (= id (get-in s [:ui :focus-title]))))
    (testing "原本的下一段改為承接新段落，關係待確認"
      (is (nil? (tree/relation-between (:relations s) "a1" "a2")))
      (is (:relation/pending (tree/relation-between (:relations s) id "a2"))))
    (testing "可以復原"
      (is (= (:nodes state) (:nodes (ui/undo s))))))
  (let [s (ui/add-sibling state "A")]
    (is (= ["A" (get-in s [:ui :selected]) "B"] (tree/child-ids (:nodes s) nil)) "新章接在後面")))

(deftest sibling-kind-test
  (let [s (ui/add-child state "B" :intro)
        intro (get-in s [:ui :selected])]
    (is (= :paragraph (ui/sibling-kind (:nodes s) intro)) "引言之後依同層卡片")
    (let [s (ui/add-sibling s intro)]
      (is (= [intro (get-in s [:ui :selected]) "b1"] (tree/child-ids (:nodes s) "B"))))))

(deftest delete-node-test
  (let [s (ui/delete-node state "A")]
    (testing "連同下層卡片一起刪除，相關關係移除"
      (is (= ["B" "b1"] (tree/document-order (:nodes s))))
      (is (empty? (:relations s))))
    (is (= "B" (get-in s [:ui :selected])))
    (is (= (:nodes state) (:nodes (ui/undo s))) "可以復原"))
  (let [s (ui/delete-node state "a1")]
    (is (= ["a2"] (tree/child-ids (:nodes s) "A")))
    (is (= "a2" (get-in s [:ui :selected])))))

(deftest scroll-column-test
  (is (= -30 (get-in (ui/scroll-column state 2 -30 -100 0) [:ui :bias 2])))
  (is (= -100 (get-in (ui/scroll-column state 2 -500 -100 0) [:ui :bias 2])) "不超出範圍")
  (is (= state (ui/edge-scroll state 2 -30 -100 0)) "沒有拖曳時邊緣不捲動"))

(deftest toggle-editor-test
  (is (true? (get-in state [:ui :editor-collapsed])) "編輯區預設收合")
  (is (false? (get-in (ui/toggle-editor state) [:ui :editor-collapsed])))
  (is (true? (get-in (ui/toggle-editor (ui/toggle-editor state)) [:ui :editor-collapsed]))))

(deftest remote-relation-test
  (let [s (ui/add-remote-relation state "b1" "a1" :basis)
        {:keys [outgoing incoming]} (tree/remote-relations (:relations s) "b1")]
    (testing "任意距離的段落都能建立關係"
      (is (= [{:relation/from "b1" :relation/to "a1" :relation/type :basis :relation/remote true}] outgoing))
      (is (empty? incoming))
      (is (= 1 (count (:incoming (tree/remote-relations (:relations s) "a1"))))))
    (testing "不重複、不能連到自己或非段落"
      (is (= (:relations s) (:relations (ui/add-remote-relation s "b1" "a1" :basis))))
      (is (= state (ui/add-remote-relation state "a1" "a1" :echo)))
      (is (= state (ui/add-remote-relation state "a1" "A" :echo))))
    (testing "不相鄰的對比不會被當成相鄰關係"
      (let [s (ui/add-remote-relation state "a1" "a2" :contrast)]
        (is (= :causal (:relation/type (tree/relation-between (:relations s) "a1" "a2"))))))
    (testing "移除"
      (is (empty? (:outgoing (tree/remote-relations (:relations (ui/remove-remote-relation s "b1" "a1" :basis)) "b1")))))
    (testing "移動段落時不影響不相鄰的關係"
      (let [moved (ui/move-selected (ui/select s "a2") -1)]
        (is (= outgoing (:outgoing (tree/remote-relations (:relations moved) "b1"))))))))

(deftest open-close-editor-test
  (let [closed (ui/close-editor state)]
    (is (true? (get-in closed [:ui :editor-collapsed])))
    (is (false? (get-in (ui/open-editor closed) [:ui :editor-collapsed])))
    (is (true? (get-in (ui/select closed "a2") [:ui :editor-collapsed])) "選取卡片不會自動打開")
    (is (false? (get-in (ui/add-sibling closed "a2") [:ui :editor-collapsed])) "新增卡片時打開以輸入名稱")))

(deftest previous-test
  (let [s (ui/toggle-previous state)]
    (is (true? (get-in s [:ui :show-previous])))
    (is (= {:id "a2" :data {}} (get-in (ui/set-previous s "a2" {}) [:ui :previous])))))

(deftest set-prose-test
  (let [doc {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "新的本文"}]}]}]
    (testing "AI 生成的段落改了本文就轉為人工改過"
      (let [s (ui/set-prose state "a1" doc)]
        (is (= doc (get-in s [:nodes "a1" :node/prose])))
        (is (= :human (get-in s [:nodes "a1" :node/status])))
        (is (= (:history state) (:history s)) "逐字輸入不進復原紀錄")))
    (testing "尚未生成的段落直接撰寫也算人工"
      (is (= :human (get-in (ui/set-prose state "b1" doc) [:nodes "b1" :node/status]))))
    (testing "鎖定的段落不能修改"
      (let [locked (assoc-in state [:nodes "a1" :node/status] :locked)]
        (is (identical? locked (ui/set-prose locked "a1" doc)))))
    (testing "引言不套用段落狀態"
      (let [s (ui/add-child state "A" :intro)
            intro (get-in s [:ui :selected])]
        (is (nil? (get-in (ui/set-prose s intro doc) [:nodes intro :node/status])))))))

(deftest relations-edit-test
  (let [s (ui/toggle-relations-edit state)]
    (is (not (get-in state [:ui :editing-relations])) "關係預設唯讀")
    (is (true? (get-in s [:ui :editing-relations])))
    (is (nil? (get-in (-> s ui/open-rel-picker ui/toggle-relations-edit) [:ui :rel-picker]))
        "完成時關閉關係選取器")
    (is (false? (get-in (ui/select s "b1") [:ui :editing-relations])) "換卡片時回到唯讀")))

(deftest set-remote-relation-type-test
  (let [s (ui/add-remote-relation state "b1" "a1" :echo)
        changed (ui/set-remote-relation-type s "b1" "a1" :echo :basis)
        remote #(set (map :relation/type (:outgoing (tree/remote-relations (:relations %) "b1"))))]
    (is (= #{:basis} (remote changed)) "呼應改為依據")
    (is (= (inc (count (:history s))) (count (:history changed))) "一次可復原的修改")
    (testing "已有該類型時合併"
      (let [both (ui/add-remote-relation s "b1" "a1" :basis)]
        (is (= #{:basis} (remote (ui/set-remote-relation-type both "b1" "a1" :echo :basis))))))
    (is (identical? s (ui/set-remote-relation-type s "b1" "a1" :echo :echo)))))

(deftest apply-versions-test
  (let [a (random-uuid) b (random-uuid)
        state {:nodes {a {:node/id a :node/version 1}}}]
    (is (= {:nodes {a {:node/id a :node/version 3}}}
           (ui/apply-versions state {a 3 b 2}))
        "送出期間已刪除的卡片略過")))
