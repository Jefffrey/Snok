(ns snok.game
  (:require [snok.snake :as s])
  (:use [snok.debug :only (dbg)]))

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
  (new Game 0 (s/make pos [15.0 0.0]) []))

; Moves the snake.
(defn update [dlt gm]
  (let [new-gm (update-in gm [:snake] (partial s/update dlt))]
    (dbg new-gm)))

(defn update-left [gm]
  (update-in gm [:snake] (partial s/direct :left)))

(defn update-right [gm]
  (update-in gm [:snake] (partial s/direct :right)))

; ---------------------------------------
; Rendering
; ---------------------------------------

; Forwards the drawing of the snake.
(defn draw [g gm]
  (s/draw g (:snake gm)))
