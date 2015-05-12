(ns snok.core
  (:use [snok.view :only (main-loop)]
        [snok.game :as g]))

(def controls
  [ ["A" update-left]
  , ["D" update-right] ])

(defn -main []
  (main-loop 
    g/update
    g/draw
    controls
    (g/start [150.0 150.0])))
