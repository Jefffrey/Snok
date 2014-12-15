{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Snok.Game
    ( Game
    , Except(..)
    , start
    , update
    , handle
    ) where

import Data.Typeable
import Control.Exception (Exception, throw)
import Snok.Types (Distance, Direction, Position, Drawable, draw)
import Snok.Math (Angle, Degrees(..), Vec2(..))
import Snok.Player (Player)
import Control.Lens
import Graphics.Gloss.Interface.Pure.Game
import qualified Snok.Player as Player

data Except = EndGame deriving (Show, Typeable)
instance Exception Except 

data Game = Game { _player :: Player
                 } deriving (Eq, Show)
makeLenses ''Game

start :: Game
start =
    Game { _player = Player.make (Vec2 0 0) (Degrees 0)
         }

update :: Float -> Game -> Game
update dt = over player (Player.update dt)

onKey :: Key -> KeyState -> Game -> Game
onKey (SpecialKey KeyEsc) Down = const (throw EndGame)
onKey (Char 'a') Down = over player Player.rotateLeft
onKey (Char 'a') Up = over player Player.rotateRight
onKey (Char 'd') Down = over player Player.rotateRight
onKey (Char 'd') Up = over player Player.rotateLeft
onKey (Char 'x') Down = over player Player.extend
onKey _ _ = id


handle :: Event -> Game -> Game
handle (EventKey k s _ _) = onKey k s
handle _ = id

instance Drawable Game where
    draw g = draw $ g ^. player 
