(ns snok.graphics
  (:use [seesaw.graphics :only (draw circle style)]))

(defn draw-game [g s] 
  (draw g 
    (circle 100 100 s)
    (style :foreground :blue :background :green :font :monospace)))
