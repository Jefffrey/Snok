{-# LANGUAGE TemplateHaskell #-}

module Snok.Snake
    ( Snake
    , spawn
    , move
    , direct
    , rotate
    ) where

import Control.Applicative (liftA2, pure)
import Graphics.Gloss hiding (rotate)
import Snok.Types (Unit, Position, Direction, Distance, Drawable, draw)
import Snok.Math (Vec2(..), Angle(..), normalize, vecRotation, toPair)
import Control.Lens

data Segment =
    Segment { _direction :: Direction 
            , _position  :: Position }
    deriving (Eq, Show)
makeLenses ''Segment

data Snake = Snake { _segments :: [Segment] } deriving (Eq, Show)
makeLenses ''Snake

originDirection :: Direction
originDirection = Vec2 0 1

toOriginDirection :: Snake -> Snake
toOriginDirection = (segments . _head . direction) .~ originDirection

spawn :: (Angle a) => Position -> a Unit -> Snake
spawn p a = 
    let nd = vecRotation a originDirection
    in  Snake [(Segment nd p)]

moveSegment :: Distance -> Segment -> Segment
moveSegment d sg =
    let op = sg ^. position
        ov = liftA2 (*) (sg ^. direction) (pure d)
        np = liftA2 (+) op ov
    in  position .~ np $ sg

move :: Distance -> Snake -> Snake
move d = 
    let moveAllSegments :: Distance -> [Segment] -> [Segment]
        moveAllSegments d = map (moveSegment d)
    in  over segments (moveAllSegments d)

direct :: (Angle a) => a Unit -> Snake -> Snake
direct a = rotate a . toOriginDirection

rotate :: (Angle a) => a Unit -> Snake -> Snake
rotate a = over (segments . _head . direction) (vecRotation a)

instance Drawable Segment where
    draw sg = 
        let p = sg ^. position
        in  (uncurry translate) (toPair p) $ color red $ circleSolid 5

instance Drawable Snake where
    draw s = pictures $ map draw $ s ^. segments
