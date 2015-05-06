(ns snok.core
  (:use [snok.view :only (main-loop)]
        [snok.game :as g]))

(defn -main []
  (main-loop 
    g/update
    g/draw
    (g/start [150.0 150.0])))
