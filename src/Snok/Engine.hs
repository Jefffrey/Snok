module Snok.Engine
    ( Initializable(..)
    , Drawable(..)
    , play ) where

import qualified Graphics.UI.GLFW as GLFW
import           System.Exit (exitSuccess, exitFailure)
import           System.IO (stderr, hPutStrLn, hFlush)
import           Control.Monad (when, unless)
import           Control.Concurrent.STM

data Event
    = KeyPressed GLFW.Key
    | KeyReleased GLFW.Key
    deriving (Eq)
type EventQueue = TQueue Event

withEngine :: IO () -> IO ()
withEngine action = do
    GLFW.setErrorCallback (Just errorCallback)
    successfulInit <- GLFW.init
    if successfulInit 
    then do
        action
        GLFW.terminate
        exitSuccess
    else error "Woops, engine not initialized"

data Window = Window String (Int, Int) deriving (Eq)

errorCallback :: GLFW.ErrorCallback
errorCallback err msg = do
    hPutStrLn stderr $ show err ++ msg
    hFlush stderr

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
        Nothing     -> GLFW.terminate >> error "Could not init window"
        Just window -> do
            GLFW.makeContextCurrent $ Just window
            GLFW.setKeyCallback window $ Just $ keyCallback events
            windowLoop window action
            GLFW.destroyWindow window

class Initializable t where
    initialize :: t -> IO ()

class Drawable t where
    draw :: t -> IO ()

play :: (Initializable t, Drawable t) => t -> IO ()
play obj = 
    withEngine $ do
        let win = Window "Example" (640, 480)
        initialize obj
        withWindow win $ draw obj
