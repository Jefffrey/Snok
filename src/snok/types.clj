(ns snok.types)

(defrecord snake
  [segments   ; list of points
   direction  ; versor
   speed])    ; speed in blocks/s

(defrecord game
  [score      ; integer
   snake      ; snake record
   apples])   ; list of points
