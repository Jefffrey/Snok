(ns snok.vector)

; Some helpful vector functions
(defn vec+ [[ax ay] [bx by]] [(+ ax bx) (+ ay by)])
(defn vec- [[ax ay] [bx by]] [(- ax bx) (- ay by)])
