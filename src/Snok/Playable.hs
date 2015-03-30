module Snok.Playable where

import Snok.Engine
import Snok.Window

class Initializable t where
    initialize :: t -> IO ()

class Drawable t where
    draw :: t -> IO ()

play :: (Initializable t, Drawable t) => t -> IO ()
play obj = 
    withEngine $ do
        let win = makeWindow "Example" (640, 480)
        initialize obj
        withWindow win $ draw obj
