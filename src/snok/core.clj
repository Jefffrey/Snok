(ns snok.core
  (:use [seesaw.dev]
        [seesaw.core]
        [seesaw.graphics]
        [seesaw.behave]
        [seesaw.border]
        snok.vector))

(defrecord snake
  [segments   ; list of points
   direction  ; versor
   speed])    ; speed in blocks/s
(defrecord game
  [score      ; integer
   snake      ; snake record
   apples])   ; list of points

(defn -main []
  (let [canv (canvas :background :red)
        tim (timer 
          (fn [[c b]] 
            (if b
              (config! c :background :black)
              (config! c :background :red))
            [c (not b)])
          :initial-value [canv true]
          :delay 1000)]
    (show! 
      (frame 
        :title "Snok" 
        :width 500 :height 200
        :content canv
        :on-close :exit))))
