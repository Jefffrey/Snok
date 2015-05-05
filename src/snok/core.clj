(ns snok.core
  (:use [seesaw.core :as sc]
        [seesaw.graphics :as sg]
        [snok.logic :only (update-game)]
        [snok.graphics :only (draw-game)])
  (:import [snok.types snake]))

; Creates the frame and calls `update-fn` every once in a while
; passing the delta time and the current state and expects the new state
; in return. The `draw-fn` is then called with the current context and
; current state in order to draw the state.
(defn main-loop [update-fn draw-fn ini-state]
  (let [delta 1000
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
              :delay delta)]
    (show! 
      (frame 
        :title "Snok" 
        :width 600 :height 300
        :content canv
        :on-close :exit
        :resizable? false))))

; Entry point of our program.
(defn -main []
  (main-loop 
    update-game 
    draw-game
    (new snake [[34 56] [38 63] [45 23]] [0 1] 10)))
