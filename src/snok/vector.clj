(ns snok.vector)

; Some helpful arithmetic functions.
; The < or > on one side identifies the position
; of the vector. The lack of < or > on one side defines
; that the operation works on a unit.
(defn vector-sum [[ax ay] [bx by]] [(+ ax bx) (+ ay by)])
(defn vector-sub [[ax ay] [bx by]] [(- ax bx) (- ay by)])
(defn vector-mul [[x y] k] [(* x k) (* y k)])
(defn vector-div [[x y] k] [(double (/ x k)) (double (/ y k))])

; Finds the norm of a vector.
(defn norm [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

; Returns the normalized version of the 
; passed vector.
(defn normalized [v]
  (let [n (norm v)]
    (vector-div v n)))

; Finds the versor that points from
; `from` to `to`.
(defn versor-from-to [from to]
  (normalized (vector-sub to from)))

; Finds the perpendicular vector to the right
; and to the left.
(defn perp-right [[x y]] (normalized [y (- x)]))
(defn perp-left [[x y]] (normalized [(- y) x]))

; Rotates the vector, keeping its magnitude.
(defn rotate [ang [x y]]
  (let [xn (- (* x (Math/cos ang)) (* y (Math/sin ang)))
        yn (+ (* x (Math/sin ang)) (* y (Math/cos ang)))]
    [xn yn]))
