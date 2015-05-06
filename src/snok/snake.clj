(ns snok.snake
  (:require 
    [snok.vector :as v]
    [snok.debug :only (dbg)]
    [seesaw.graphics :as ss]))

; ---------------------------------------
; Records
; ---------------------------------------

(defrecord Snake
  [segments         ; list of points
   direction        ; versor * speed (pixels/s)
   steering-angle]) ; angle or rotation in radians

; ---------------------------------------
; Logic
; ---------------------------------------

; Constructor.
(defn make [pos dir]
  (new Snake [pos] dir 0.17))

; Changes the direction of the snake
; to its left.
(defn direct-right [s]
  (update-in s [:direction] (partial v/rotated (:steering-angle s))))

; Changes the direction of the snake
; to its right.
(defn direct-left [s]
  (update-in s [:direction] (partial v/rotated (:steering-angle (- s)))))

; Returns the versor of the last segment.
(defn last-versor [s]
  (let [segs (:segments s)]
    (if (< (count segs) 2)
      [0.0 0.0]
      (let [[h o] (take 2 segs)]
        (v/versor-from-to o h)))))

; Moves the current head to a new place.
(defn replace-head [pos snk]
  (let [segs (:segments snk)
        tail (into [] (rest segs))]
    (assoc snk :segments (into [] (cons pos tail)))))

; Adds a new head, leaving the previous one
; where it is.
(defn add-head [pos snk]
  (update-in snk [:segments]
    (fn [segs]
      (into [] (cons pos segs)))))

; Moves the snake head of the amount of seconds
; necessary.
(defn move-head [dlt snk]
  (let [segs (:segments snk)
        dir (:direction snk)
        head (first segs)
        next-head (v/vector-sum head (v/vector-mul dir (double (/ dlt 1000))))]
    (if (= (last-versor snk) (v/normalized dir))
      (replace-head next-head snk)
      (add-head next-head snk))))

; @todo: move tail

; Performs the necessary operations for
; an update.
(defn update [dlt snk]
  (move-head dlt snk))

; ---------------------------------------
; Rendering
; ---------------------------------------

; Graphic configurations.
(def snake-radius 10)
(def body-stroke
  (ss/stroke 
    :width snake-radius
    :cap :round
    :join :round))
(def body-style 
  (ss/style 
    :foreground :white 
    :stroke body-stroke))

; Draws the line that represents
; the snake.
; @todo: Add head. (Yeah, now I remember why I added it in the first place.
(defn draw [g snk]
  (doall 
    (map
      (fn [[ax ay] [bx by]]
        (ss/draw g (ss/line ax ay bx by) body-style))
      (:segments snk)
      (into [] (rest (:segments snk))))))
