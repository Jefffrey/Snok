module Snok.Playable where

import Snok.Engine
import Snok.Window

class Drawable t where
    draw :: t -> IO ()

play :: (Drawable t) => t -> IO ()
play obj = 
    withEngine $ do
        let win = makeWindow "Example" (640, 480)
        withWindow win $ draw obj
