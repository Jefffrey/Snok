{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Graphics.Gloss
import Control.Exception (catch)
import Snok.Types (draw)
import Snok.Game (Except(..), Game, update, start, handle)
import Graphics.Gloss.Interface.Pure.Game

view :: Display
view = InWindow "Snok 0.2.0" (450, 300) (0, 0)

loop :: IO ()
loop = do
    game <- start
    play view black 70 game draw handle update

main :: IO ()
main = catch loop (\EndGame -> return ())
