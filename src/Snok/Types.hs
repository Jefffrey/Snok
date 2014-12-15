module Snok.Types where

import Graphics.Gloss
import Snok.Math (Vec2)

type Unit               = Float
type Distance           = Unit
type Direction          = Vec2 Unit
type Position           = Vec2 Unit
type Seconds            = Unit

class Drawable a where
    draw :: a -> Picture
