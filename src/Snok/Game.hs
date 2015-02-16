{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Snok.Game
    ( Game
    , Except(..)
    , start
    , update
    , render
    ) where

import Data.Typeable
import Control.Exception (Exception, throw)
import Snok.Types
import Snok.Classes
import Snok.Math
import Snok.Input
import Snok.Player (Player)
import Control.Lens
import System.Random (StdGen, getStdGen, randomR)
import Control.Monad.State
import qualified Snok.Player as Player
import qualified FRP.Helm.Graphics as G
import qualified FRP.Helm.Color as C

data Item = 
    Apple { 
        _position :: Position
    }
    deriving (Eq, Show)
makeLenses ''Item

data Except = EndGame deriving (Show, Typeable)

data Game = 
    Game { 
        _player :: Player,
        _items :: [Item],
        _chaos :: StdGen
    } deriving (Show)
makeLenses ''Game

instance Drawable Item where
    draw (Apple p) = G.filled C.red $ G.square 20

instance Exception Except 

start :: IO Game
start = do
    ranGen <- getStdGen
    return $
        Game { _player = Player.make (Vec2 0 0) (Degrees 0)
             , _items = [Apple (Vec2 20 30)]
             , _chaos = ranGen
             }

update :: Input -> Game -> Game
update i = execState $ do
    player %= Player.update i

render :: (Int, Int) -> Game -> G.Element
render (w, h) g = G.centeredCollage w h [draw g]

instance Drawable Game where
    draw g = 
        G.group $ concat
            [ [G.filled (C.rgb (121 / 255) (170 / 255) (68 / 255)) $ G.square 400]
            , [draw (g ^. player)]
            , map draw (g ^. items)
            ]
