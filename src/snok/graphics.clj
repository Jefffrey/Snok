(ns snok.graphics
  (:use [seesaw.graphics :only (draw polygon style)]))

(defn make-polygon [snake]
  (let [segs (:segments snake)]
    segs)) ; todo

(defn draw-snake [g snake]
  (draw g
      (polygon [10 10] [300 200] [100 400])
      (style :foreground :blue :background :green :font :monospace)))

; Forwards the drawing of the snake.
(defn draw-game [g snake]
  (draw-snake g snake))
