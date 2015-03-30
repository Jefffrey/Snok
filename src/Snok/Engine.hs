module Snok.Engine (withEngine) where

import           Snok.Log
import           System.Exit (exitSuccess, exitFailure)
import qualified Graphics.UI.GLFW as GLFW

withEngine :: IO () -> IO ()
withEngine action = do
    GLFW.setErrorCallback (Just errorCallback)
    successfulInit <- GLFW.init
    if successfulInit 
    then do
        action
        GLFW.terminate
        exitSuccess
    else do
        logError "engine not initialized"
        exitFailure

errorCallback :: GLFW.ErrorCallback
errorCallback err msg = logError $ show err ++ " " ++ msg
