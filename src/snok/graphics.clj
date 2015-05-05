(ns snok.graphics
  (:use [seesaw.graphics :only (draw line stroke style)]))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

; Graphical configurations.
(def snake-stroke
  (stroke 
    :width 10
    :cap :round
    :join :round))

(def snake-style 
  (style 
    :foreground :blue 
    :stroke snake-stroke))

; Draws the line that represents
; the snake.
(defn draw-snake [g snake]
  (doall 
    (map
      (fn [[ax ay] [bx by]]
        (draw g (line ax ay bx by) snake-style))
      (:segments snake)
      (into [] (rest (:segments snake))))))

; Forwards the drawing of the snake.
(defn draw-game [g snake]
  (draw-snake g snake))
