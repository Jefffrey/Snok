(ns snok.types)

(defrecord Snake
  [segments   ; list of points
   direction  ; versor
   speed])    ; speed in blocks/s

(defrecord Game
  [score      ; integer
   snake      ; snake record
   apples])   ; list of points
