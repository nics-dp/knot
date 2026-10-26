(ns knot.app
  (:require [knot.views :as views]
            [replicant.dom :as r]))

(defonce store (atom {}))

(defn render! [state]
  (r/render (js/document.getElementById "app") (views/app state)))

(defn reload! []
  (render! @store))

(defn init []
  (add-watch store ::render (fn [_ _ _ state] (render! state)))
  (render! @store))
