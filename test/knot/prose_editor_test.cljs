(ns knot.prose-editor-test
  (:require [clojure.test :refer [deftest is]]
            [knot.prose-editor :as prose-editor]))

(deftest json->edn-test
  ;; ProseMirror 的 attrs 沒有原型，js->clj 不會轉換（曾造成存檔時 HTTP 500）
  (let [attrs (doto (js/Object.create nil) (aset "key" "k.kept"))
        json #js {:type "doc"
                  :content #js [#js {:type "paragraph"
                                     :content #js [#js {:type "fact" :attrs attrs}]}]}
        edn (prose-editor/json->edn json)]
    (is (= {:type "doc" :content [{:type "paragraph" :content [{:type "fact" :attrs {:key "k.kept"}}]}]}
           edn))
    (is (not (re-find #"#object" (pr-str edn))))))
