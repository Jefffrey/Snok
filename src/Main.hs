module Main where

import Snok.Engine

main :: IO ()
main = do
    let win = Window "Example" (640, 480)
    withEngine $ withWindow win $
        return ()
