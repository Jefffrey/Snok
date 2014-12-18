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
import qualified Snok.Box as Box
import qualified Snok.Player as Player
import qualified FRP.Helm.Graphics as G
import qualified FRP.Helm.Color as C

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
    draw (Apple p r) = G.filled C.red $ G.circle r

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

update :: Input -> Game -> Game
update i = execState $ do
    player %= Player.update i 

render :: (Int, Int) -> Game -> G.Element
render (w, h) g = G.centeredCollage w h [draw g]

instance Drawable Game where
    draw g = 
        G.group $ concat
            [ [draw (g ^. player)]
            , map draw (g ^. items)
            ]
