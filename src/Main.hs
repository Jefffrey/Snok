module Main where

import           Snok.Playable
import           Snok.Log
import           Graphics.Rendering.OpenGL (($=))
import           Control.Applicative
import           System.FilePath ((</>))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU

shaderPath :: String
shaderPath = "shaders/"

vertices :: [Float]
vertices = [  0.0,  0.8
           , -0.8, -0.8
           ,  0.8, -0.8
           ]

initGame :: IO Game
initGame = return $ Game ()

newtype Game = Game ()
instance Drawable Game where
    prepare (Game ()) = do
        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        logDebug "before error"
        vs <- GLU.loadShader GL.VertexShader $ shaderPath </> "a.vs"
        logDebug "after error"
        fs <- GLU.loadShader GL.FragmentShader $ shaderPath </> "a.fs"
        p <- GLU.linkShaderProgram [vs, fs]
        Program
            <$> return p
            <*> GL.get (GL.attribLocation p "vPosition")
            <*> GLU.makeBuffer GL.ArrayBuffer vertices

    draw (Game ()) (Program program attrib buf) = do
        GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
        GL.clear [GL.ColorBuffer]
        GL.currentProgram $= Just program
        GL.vertexAttribArray attrib $= GL.Enabled
        GL.bindBuffer GL.ArrayBuffer $= Just buf
        GL.vertexAttribPointer attrib $=
            (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 GLU.offset0)
        GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
        GL.vertexAttribArray attrib $= GL.Disabled

main :: IO ()
main = initGame >>= play
