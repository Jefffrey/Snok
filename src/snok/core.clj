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
      :title "HI!" 
      :content "I'm a label!" 
      :width 500 :height 600)))

(-main)
