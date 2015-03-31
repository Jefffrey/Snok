module Snok.Window
    ( Window
    , makeWindow
    , withWindow
    ) where

import           Snok.Log
import           Control.Concurrent.STM
import           Control.Monad (when, unless)
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

data Window = Window String (Int, Int) deriving (Eq)

makeWindow :: String -> (Int, Int) -> Window
makeWindow = Window

data Event
    = KeyPressed GLFW.Key
    | KeyReleased GLFW.Key
    deriving (Eq)
type EventQueue = TQueue Event

windowSizeCallback :: GLFW.WindowSizeCallback
windowSizeCallback _ w h =
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

keyCallback :: EventQueue -> GLFW.KeyCallback
keyCallback events _ key _ state _ =
    let event = (case state of
            GLFW.KeyState'Pressed -> KeyPressed key
            GLFW.KeyState'Released -> KeyReleased key)
    in atomically $ writeTQueue events event

windowLoop :: GLFW.Window -> IO () -> IO ()
windowLoop window action = do
    action
    b <- GLFW.windowShouldClose window
    unless b $ do
        GLFW.pollEvents
        GLFW.swapBuffers window
        windowLoop window action

withWindow :: Window -> IO a -> (a -> IO ()) -> IO ()
withWindow (Window title (w, h)) init action = do
    GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    logDebug "window hints set"
    events  <- (newTQueueIO :: IO EventQueue)
    win     <- GLFW.createWindow w h title Nothing Nothing
    case win of
        Nothing     -> do
            GLFW.terminate
            logError "window not initialized"
        Just window -> do
            logDebug "window created"
            GLFW.makeContextCurrent $ Just window
            logDebug "context selected"
            windowSizeCallback window w h
            logDebug "initial window resize callback"
            GLFW.setKeyCallback window $ Just (keyCallback events)
            GLFW.setWindowSizeCallback window $ Just windowSizeCallback
            logDebug "window callbacks set"
            st <- init
            logDebug "drawing state initialized"
            windowLoop window (action st)
            GLFW.destroyWindow window
            logDebug "window destroyed"
