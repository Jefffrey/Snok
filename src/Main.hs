module Main where

import           Snok.Engine
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

newtype Game = Game ()

instance Initializable Game where
    initialize _ = return ()

instance Drawable Game where
    draw _ =
        GL.clear [GL.ColorBuffer]

main :: IO ()
main = play (Game ())
