(ns snok.core
  (:use [seesaw.core :as sc]
        [seesaw.graphics :as sg]
        [snok.snake :as snk]
        [snok.graphics :only (draw-game)]
        [seesaw.keymap :only (map-key)])
  (:import [snok.snake Snake]))

; Creates the frame and calls `update-fn` every once in a while
; passing the delta time and the current state and expects the new state
; in return. The `draw-fn` is then called with the current context and
; current state in order to draw the state.
(defn main-loop [update-fn draw-fn ini-state]
  (let [delta 100
        state (atom ini-state)
        canv (canvas
               :background :black
               :paint 
                (fn [_ gcxt]
                  (anti-alias gcxt)
                  (draw-fn gcxt @state)))
        tmr (timer ; update & draw timer
             (fn [_] 
               (swap! state (partial update-fn delta))
               (repaint! canv))
              :delay delta)
        frm (frame 
          :title "Snok" 
          :width 900 :height 400
          :content canv
          :on-close :exit
          :resizable? false)]
    (show! frm)))

; Entry point of our program.
(defn -main []
  (main-loop 
    snk/move
    draw-game
    (new Snake [[150.0 100.0] [200.0 145.0]] [0.0 15.0] 1)))
