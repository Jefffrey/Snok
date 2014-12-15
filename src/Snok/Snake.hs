{-# LANGUAGE TemplateHaskell #-}

module Snok.Snake
    ( Snake
    , spawn
    , move
    , followSegment
    , Segment (..)
    , rotate
    , extend
    ) where

import Control.Applicative (liftA2, pure)
import Graphics.Gloss hiding (rotate)
import Snok.Types (Unit, Position, Direction, Distance, Drawable, draw)
import Snok.Math (Vec2(..), Angle(..), normalize, vecRotation, toPair, magnitude)
import Snok.Utils (inPairs)
import Control.Lens
import Debug.Trace (traceShowId)

data Segment =
    Segment { _direction :: Direction 
            , _position  :: Position
            , _radius    :: Unit }
    deriving (Eq, Show)
makeLenses ''Segment

data Snake = Snake { _segments :: [Segment] 
                   , _segmentDistance :: Unit  -- prolly don't need it
                   } deriving (Eq, Show)
makeLenses ''Snake

originDirection :: Direction
originDirection = Vec2 0 1

toOriginDirection :: Snake -> Snake
toOriginDirection = (segments . _head . direction) .~ originDirection

spawn :: (Angle a) => Position -> a Unit -> Snake
spawn p a = 
    let nd = vecRotation a originDirection
    in  Snake [(Segment nd p 8)] 10

extend :: Snake -> Snake
extend s =
    let (Just t) = s ^. segments ^? _last
        sd = s ^. segmentDistance
        np = liftA2 (-) (t ^. position) (fmap (* sd) (t ^. direction))
        ns = Segment (t ^. direction) np 8
    in  over segments (++ [ns]) s

followSegment :: Segment -> Segment -> Segment
followSegment t s =
    let tp = t ^. position
        sp = s ^. position
        tv = tp - sp
    in  over direction (\a -> normalize (a + tv)) s

applyFriction :: Distance -> Segment -> Segment -> Distance
applyFriction d t s =
    let id = t ^. radius + s ^. radius
        ad = magnitude (t ^. position - s ^. position)
    in  d * (ad / id)

moveHead :: Distance -> [Segment] -> [Segment]
moveHead d sgs =
    let (Just h) = sgs ^? _head
        op = h ^. position
        ov = liftA2 (*) (h ^. direction) (pure d)
        np = liftA2 (+) op ov
    in  (_head . position) .~ np $ sgs

moveSegment :: Distance -> Segment -> Segment -> Segment
moveSegment d t s =
    let op = s ^. position
        ov = liftA2 (*) (s ^. direction) (pure (applyFriction d t s))
        np = liftA2 (+) op ov
    in  position .~ np $ s

move :: Distance -> Snake -> Snake
move d = over segments (scanl1 (moveSegment d) . (moveHead d) . scanl1 followSegment)

rotate :: (Angle a) => a Unit -> Snake -> Snake
rotate a = over (segments . _head . direction) (vecRotation a)

instance Drawable Segment where
    draw sg = 
        let r = sg ^. radius
            p = sg ^. position
            d = sg ^. direction
            pa = p + (fmap ((*(1/3)) . (*r)) d)
            pb = p - (fmap ((*(1/3)) . (*r)) d)
        in pictures . map (\v -> (uncurry translate) (toPair v) $ color blue $ circleSolid r) $ [p, pb, pa]

instance Drawable Snake where
    draw s = pictures $ map draw $ s ^. segments
