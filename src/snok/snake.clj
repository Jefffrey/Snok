(ns snok.snake
  (:use [snok.vector :as v]
        [snok.debug :only (dbg)]))

(defrecord Snake
  [segments         ; list of points
   direction        ; versor * speed (pixels/s)
   steering-angle]) ; angle or rotation in radians

; Changes the direction of the snake
; to its left.
(defn direct-right [s]
  (update-in s [:direction] (partial v/rotate (:steering-angle s))))

; Changes the direction of the snake
; to its right.
(defn direct-left [s]
  (update-in s [:direction] (partial v/rotate (:steering-angle (- s)))))

; Returns the versor of the last segment.
(defn last-segment-versor [s]
  (let [segs (:segments s)]
    (if (< (count segs) 2)
      [0.0 0.0]
      (let [[h o] (take 2 segs)]
        (v/versor-from-to o h)))))

; Moves the snake of the amount of seconds
; necessary.
(defn move [dlt snk]
  (let [segs (:segments snk)
        dir (:direction snk)
        head (first segs)
        tail (into [] (rest segs))
        next-head (v/vector-sum head (v/vector-mul dir (double (/ dlt 1000))))]
    (if (dbg (= (dbg (last-segment-versor snk)) (dbg (v/normalized dir))))
      (assoc snk :segments (into [] (cons (dbg next-head) (dbg tail))))
      (assoc snk :segments (into [] (cons (dbg next-head) (dbg segs)))))))
