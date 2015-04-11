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
        GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
        vs <- GLU.loadShader GL.VertexShader $ shaderPath </> "a.vs"
        fs <- GLU.loadShader GL.FragmentShader $ shaderPath </> "a.fs"
        p <- GLU.linkShaderProgram [vs, fs]
        Program
            <$> return p
            <*> GLU.makeBuffer GL.ArrayBuffer vertices
            <*> GL.genObjectName -- vao

    draw (Game ()) (Program program vbo vao) = do
        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        GL.clear [GL.ColorBuffer]
        GL.currentProgram $= Just program
        GL.bindBuffer GL.ArrayBuffer $= Just vbo
        GL.bindVertexArrayObject $= Just vao
        frustrumScaleAttr <- GL.get (GL.uniformLocation program "ng_FrustumScale")
        zNearAttr <- GL.get (GL.uniformLocation program "ng_ZNear")
        zFarAttr <- GL.get (GL.uniformLocation program "ng_ZFar")
        offset <- GL.get (GL.uniformLocation program "ng_Offset")
        posAttr <- GL.get (GL.attribLocation program "ng_Position")
        colorAttr <- GL.get (GL.uniformLocation program "ng_Color")
        GL.uniform frustrumScaleAttr $= GL.Index1 (1.0 :: GL.GLfloat)
        GL.uniform offset $= GL.Vertex2 (0.50 :: GL.GLfloat) (-0.25)
        GL.uniform zNearAttr $= GL.Index1 (1.0 :: GL.GLfloat)
        GL.uniform zFarAttr $= GL.Index1 (3.0 :: GL.GLfloat)
        GL.uniform colorAttr $= GL.Vertex4 (1.0 :: GL.GLfloat) 0.0 0.0 1.0
        GL.vertexAttribArray posAttr $= GL.Enabled
        GL.vertexAttribPointer posAttr $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 GLU.offset0)
        GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
        GL.vertexAttribArray posAttr $= GL.Disabled

main :: IO ()
main = initGame >>= play
