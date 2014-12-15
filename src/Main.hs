{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Graphics.Gloss
import Control.Exception (catch)
import Snok.Math (Vec2(..), Degrees(..))
import Snok.Game (Except(..), Game, update, start, handle)
import Snok.Types (draw)
import Graphics.Gloss.Interface.Pure.Game
import qualified Snok.Game as Game

startGame :: Game
startGame = start

view :: Display
view = InWindow "Snok 0.1.0" (450, 300) (0, 0)

loop :: IO ()
loop = play view black 70 startGame draw handle update

main :: IO ()
main = catch loop (\EndGame -> return ())
