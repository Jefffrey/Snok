(ns snok.vector)

; Some helpful arithmetic functions.
(defn + [[ax ay] [bx by]] [(+ ax bx) (+ ay by)])
(defn - [[ax ay] [bx by]] [(- ax bx) (- ay by)])

; Finds the norm of a vector.
(defn norm [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

; Returns the normalized version of the 
; passed vector.
(defn normalized [[x y]]
  (let [n (norm [x y])]
    [(/ x n) (/ y n)]))

; Finds the versor that points from
; `from` to `to`.
(defn versor-from-to [from to]
  (normalized (- to from)))
