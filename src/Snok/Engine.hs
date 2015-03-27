module Snok.Engine where

import qualified Graphics.UI.GLFW as GLFW
import           System.Exit (exitSuccess, exitFailure)
import           System.IO (stderr, hPutStrLn)
import           Control.Monad (when, unless)

withEngine :: IO () -> IO ()
withEngine action = do
    GLFW.setErrorCallback (Just errorCallback)
    successfulInit <- GLFW.init
    if successfulInit 
    then do
        action
        GLFW.terminate
        exitSuccess
    else exitFailure
    where errorCallback :: GLFW.ErrorCallback
          errorCallback err = hPutStrLn stderr

data Window = Window String (Int, Int) deriving (Eq)

keyCallback :: GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback window key _ state _ =
    when (key == GLFW.Key'Escape && state == GLFW.KeyState'Pressed) $
        GLFW.setWindowShouldClose window True

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
    GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
    win <- GLFW.createWindow w h title Nothing Nothing
    case win of
        Nothing -> GLFW.terminate >> exitFailure
        Just window -> do
            GLFW.makeContextCurrent (Just window)
            GLFW.setKeyCallback window (Just keyCallback)
            windowLoop window action
            GLFW.destroyWindow window

class Drawable t where
    render :: t -> IO ()
