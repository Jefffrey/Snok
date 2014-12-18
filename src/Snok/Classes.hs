module Snok.Classes where

import Snok.Box (Box)
import Snok.Math (distance)
import Graphics.Gloss
import qualified Snok.Box as Box

class Drawable a where
    draw :: a -> Picture

class Collidable a where
    collisionBox :: a -> Box

collides :: (Collidable a, Collidable b) => a -> b -> Bool
collides a b = Box.collides (collisionBox a) (collisionBox b)
