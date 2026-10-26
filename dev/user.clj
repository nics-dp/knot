(ns user
  (:require [integrant.repl :as ir]
            [integrant.repl.state :as state]
            [knot.system :as system]))

(ir/set-prep! (fn [] (system/config)))

(defn go [] (ir/go))
(defn halt [] (ir/halt))
(defn reset [] (ir/reset))

(defn system [] state/system)

;; (seed!)、(db) 於 M1 加入；(import-file! "path") 於 M3 加入
