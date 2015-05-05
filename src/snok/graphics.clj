(ns snok.graphics
  (:use [seesaw.graphics :only (draw line circle stroke style)]))

; Graphical configurations.
(def snake-radius 10)
(def body-stroke
  (stroke 
    :width snake-radius
    :cap :round
    :join :round))

(def body-style 
  (style 
    :foreground :grey 
    :stroke body-stroke))

(def head-style
  (style
    :background :blue))

; Draws the line that represents
; the snake.
(defn draw-snake [g snake]
  (doall 
    (map
      (fn [[ax ay] [bx by]]
        (draw g (line ax ay bx by) body-style))
      (:segments snake)
      (into [] (rest (:segments snake)))))
  (let [[hx hy] (first (:segments snake))]
    (draw g (circle hx hy (/ snake-radius 2)) head-style)))

; Forwards the drawing of the snake.
(defn draw-game [g snake]
  (draw-snake g snake))