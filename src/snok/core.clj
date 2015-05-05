(ns snok.core
  (:use seesaw.core
        snok.vector))

(defrecord snake
  [segments   ; list of points
   direction  ; versor
   speed])    ; speed in blocks/s
(defrecord game
  [score      ; integer
   snake      ; snake record
   apples])   ; list of points

(defn -main []
  (show! 
    (frame 
      :title "Snok" 
      :width 500 :height 200
      :on-close :exit)))
