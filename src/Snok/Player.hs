{-# LANGUAGE TemplateHaskell #-}

module Snok.Player where

import Snok.Math (Angle, Degrees(..))
import Snok.Snake (Snake)
import Snok.Types (Position, Unit, Distance)
import Snok.Classes (Drawable, draw)
import Control.Lens
import qualified Snok.Snake as Snake

type Speed      = Distance -- per second
type Rotation   = Degrees Unit -- per second

data Player
    = Player { _score           :: Int
             , _snake           :: Snake
             , _speed           :: Speed
             , _bendingAngle    :: Degrees Unit
             , _rotation        :: Rotation
             } deriving (Eq, Show)
makeLenses ''Player

make :: (Angle a) => Position -> a Unit -> Player
make p d =
    Player { _score         = 0
           , _snake         = Snake.spawn p d
           , _speed         = 120
           , _bendingAngle  = Degrees 220
           , _rotation      = Degrees 0
           }

extend :: Player -> Player
extend = over snake Snake.extend

rotateLeft :: Player -> Player
rotateLeft p = 
    let ba = p ^. bendingAngle
    in  over rotation (+ ba) p

rotateRight :: Player -> Player
rotateRight p = 
    let ba = p ^. bendingAngle
    in  over rotation (subtract ba) p

update :: Float -> Player -> Player 
update dt p = 
    let d = (p ^. speed) * dt
        a = fmap (* dt) (p ^. rotation)
    in  over snake (Snake.move d . Snake.rotate a) p

instance Drawable Player where
    draw p = draw $ p ^. snake
