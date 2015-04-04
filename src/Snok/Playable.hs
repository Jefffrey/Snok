module Snok.Playable where

import           Snok.Engine
import           Snok.Window
import qualified Graphics.Rendering.OpenGL as GL

data Program = Program GL.Program GL.BufferObject GL.VertexArrayObject
type DrawState = Program

class Drawable t where
    prepare :: t -> IO DrawState
    draw :: t -> DrawState -> IO ()

play :: (Drawable t) => t -> IO ()
play obj = 
    withEngine $ do
        let win = makeWindow "Example" (640, 480)
        withWindow win (prepare obj) (draw obj)
