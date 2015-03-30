module Snok.Window
    ( Window
    , makeWindow
    , withWindow
    ) where

import           Snok.Log
import           Control.Concurrent.STM
import           Control.Monad (when, unless)
import qualified Graphics.UI.GLFW as GLFW

data Window = Window String (Int, Int) deriving (Eq)

makeWindow :: String -> (Int, Int) -> Window
makeWindow = Window

data Event
    = KeyPressed GLFW.Key
    | KeyReleased GLFW.Key
    deriving (Eq)
type EventQueue = TQueue Event

keyCallback :: EventQueue -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
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

withWindow :: Window -> IO () -> IO ()
withWindow (Window title (w, h)) action = do
    GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    events  <- (newTQueueIO :: IO EventQueue)
    win     <- GLFW.createWindow w h title Nothing Nothing
    case win of
        Nothing     -> do
            GLFW.terminate
            logError "window not initialized"
        Just window -> do
            GLFW.makeContextCurrent $ Just window
            GLFW.setKeyCallback window $ Just $ keyCallback events
            windowLoop window action
            GLFW.destroyWindow window
