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
import Snok.Types
import Snok.Classes
import Snok.Math
import Snok.Player (Player)
import Control.Lens
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.Random (StdGen, getStdGen, randomR)
import Control.Monad.State
import qualified Snok.Box as Box
import qualified Snok.Player as Player

data Item
    = Apple 
        { _position :: Position
        , _radius   :: Dimension
        } 
    deriving (Eq, Show)
makeLenses ''Item

data Except = EndGame deriving (Show, Typeable)

data Game = Game { _player :: Player
                 , _items :: [Item]
                 , _chaos :: StdGen
                 } deriving (Show)
makeLenses ''Game

instance Drawable Item where
    draw (Apple p r) = color red $ circle r

instance Collidable Item where
    collisionBox a = Box.Circle (a ^. position) (a ^. radius)

instance Exception Except 

start :: IO Game
start = do
    ranGen <- getStdGen
    return $
        Game { _player = Player.make (Vec2 0 0) (Degrees 0)
             , _items = []
             , _chaos = ranGen
             }

update :: Float -> Game -> Game
update dt = execState $ do
    player %= Player.update dt
            
onKey :: Key -> KeyState -> Game -> Game
onKey (SpecialKey KeyEsc) Down = const (throw EndGame)
onKey (Char c) Down
    | c `elem` ['a', 'A'] = over player Player.rotateLeft
    | c `elem` ['d', 'D'] = over player Player.rotateRight
    | c `elem` ['x', 'X'] = over player Player.extend
    | otherwise           = id
onKey (Char c) Up
    | c `elem` ['a', 'A'] = over player Player.rotateRight
    | c `elem` ['d', 'D'] = over player Player.rotateLeft
    | otherwise           = id
onKey _ _ = id

handle :: Event -> Game -> Game
handle (EventKey k s _ _) = onKey k s
handle _ = id

instance Drawable Game where
    draw g = 
        pictures $ concat
            [ [draw (g ^. player)]
            , map draw (g ^. items)
            ]
