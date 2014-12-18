module Snok.Box where

import Snok.Types (Unit, Position, Radius)
import Snok.Math (distance)

data Box
    = Circle Position Radius
    deriving (Eq, Show)

collides :: Box -> Box -> Bool
collides (Circle ap ar) (Circle bp br) =
    (distance ap bp) < (ar + br)
