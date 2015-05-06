(ns snok.game
  (:require [snok.snake :as s]))

; ---------------------------------------
; Records
; ---------------------------------------

(defrecord Game
  [score      ; integer
   snake      ; snake record
   apples])   ; list of points

; ---------------------------------------
; Logic
; ---------------------------------------

; Defines the start of the game.
(defn start [pos]
  (new Game 0 (s/make pos [0.0 15.0]) []))

; Moves the snake.
(defn update [dlt gm]
  (update-in gm [:snake] (partial s/update dlt)))

; ---------------------------------------
; Rendering
; ---------------------------------------

; Forwards the drawing of the snake.
(defn draw [g gm]
  (s/draw g (:snake gm)))
