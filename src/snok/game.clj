(ns snok.game)

(defrecord Game
  [score      ; integer
   snake      ; snake record
   apples])   ; list of points
