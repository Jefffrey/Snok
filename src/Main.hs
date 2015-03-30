module Main where

import           Snok.Playable
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

data Program = Program GL.Program GL.AttribLocation GL.BufferObject

initGame :: IO Game
initGame = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    vs <- GLU.loadShader GL.VertexShader $ shaderPath </> "a.vs"
    fs <- GLU.loadShader GL.FragmentShader $ shaderPath </> "a.fs"
    p <- GLU.linkShaderProgram [vs, fs]
    program <-
        Program
            <$> GLU.linkShaderProgram [vs, fs]
            <*> GL.get (GL.attribLocation p "coord2d")
            <*> GLU.makeBuffer GL.ArrayBuffer vertices
    return $ Game program

newtype Game = Game Program

instance Drawable Game where
    draw (Game (Program program attrib buf)) = do
        GL.clearColor $= GL.Color4 1 1 1 1
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
