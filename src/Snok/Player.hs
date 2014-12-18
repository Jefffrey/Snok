{-# LANGUAGE TemplateHaskell #-}

module Snok.Player where

import Snok.Math (Angle, Degrees(..))
import Snok.Snake (Snake)
import Snok.Input
import Snok.Types (Position, Unit, Distance)
import Snok.Classes (Drawable, draw)
import Control.Lens
import FRP.Helm.Time (second)
import qualified FRP.Helm.Keyboard as K
import qualified Snok.Snake as Snake
import Debug.Trace (traceShowId)

type Speed      = Distance -- per second
type Rotation   = Degrees Unit -- per second

data Player
    = Player { _score           :: Int
             , _snake           :: Snake
             , _speed           :: Speed
             , _bendingAngle    :: Degrees Unit
             } deriving (Eq, Show)
makeLenses ''Player

make :: (Angle a) => Position -> a Unit -> Player
make p d =
    Player { _score         = 0
           , _snake         = Snake.spawn p d
           , _speed         = 120
           , _bendingAngle  = Degrees 220
           }

extend :: Player -> Player
extend = over snake Snake.extend

rotationFor :: Player -> K.Key -> Rotation
rotationFor p K.AKey = (- p^.bendingAngle)
rotationFor p K.DKey = (p^.bendingAngle)
rotationFor _ _      = Degrees 0

calcRotation :: Player -> Input -> Rotation
calcRotation p i = 
    foldr (\k a -> a + (rotationFor p k)) (Degrees 0) (i^.pressedKeys)

applyRotation :: Input -> Player -> Player
applyRotation i p =
    let ang = fmap (* i^.delta) (calcRotation p i)
    in  over snake (Snake.rotate ang) p

applyMovement :: Input -> Player -> Player
applyMovement i p =
    let dist = (p^.speed) * (i^.delta)
    in  over snake (Snake.move dist) p

applyExtension :: Input -> Player -> Player
applyExtension i
    | K.XKey `elem` (i^.pressedKeys) = over snake Snake.extend
    | otherwise = id

update :: Input -> Player -> Player 
update i = applyMovement i . applyRotation i . applyExtension i

instance Drawable Player where
    draw p = draw $ p^.snake
