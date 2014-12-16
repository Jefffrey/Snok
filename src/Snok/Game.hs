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
import Snok.Math
import Snok.Player (Player)
import Snok.Apple (Apple)
import Control.Lens
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)
import Control.Monad.State
import qualified Snok.Player as Player
import qualified Snok.Apple as Apple

data Except = EndGame deriving (Show, Typeable)
instance Exception Except 

data Game = Game { _player :: Player
                 , _apples :: [Apple]
                 , _chaos :: StdGen
                 , _radius :: Distance
                 } deriving (Show)
makeLenses ''Game

start :: IO Game
start = do
    ranGen <- getStdGen
    return $ spawnNewApple $
        Game { _player = Player.make (Vec2 0 0) (Degrees 0)
             , _apples = []
             , _chaos = ranGen
             , _radius = 100
             }

spawnNewApple :: Game -> Game
spawnNewApple = execState $ do
    g <- get
    let gen = g ^. chaos
    let rad = g ^. radius
    let (appX, newGen) = randomR (-rad, rad) gen
    let (appY, newGen') = randomR (-rad, rad) newGen
    chaos .= newGen'
    apples .= [Apple.spawn (Vec2 appX appY)]

update :: Float -> Game -> Game
update dt = execState $ do
    player %= Player.update dt
            
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
    draw g = 
        pictures $ concat
            [ [draw (g ^. player)]
            , map draw (g ^. apples)
            ]
