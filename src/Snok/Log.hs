module Snok.Log (logError, logDebug) where

import System.IO (stderr, stdout, hPutStrLn, hFlush)

logError :: String -> IO ()
logError err = do
    hPutStrLn stderr err
    hFlush stderr

logDebug :: String -> IO ()
logDebug msg = do
    hPutStrLn stdout msg
    hFlush stdout
