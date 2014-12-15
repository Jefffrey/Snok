{-# LANGUAGE TemplateHaskell #-}

module Snok.Snake
    ( Snake
    , spawn
    , move
    , rotate
    , extend
    ) where

import Control.Applicative (pure)
import Graphics.Gloss hiding (rotate)
import Snok.Types
import Snok.Math
import Snok.Utils (inPairs)
import Control.Lens

data Segment =
    Segment { _direction :: Direction 
            , _position  :: Position
            , _radius    :: Dimension 
            } deriving (Eq, Show)
makeLenses ''Segment

data Snake = Snake { _segments   :: [Segment] 
                   , _headRadius :: Dimension
                   , _bodyRadius :: Dimension
                   } deriving (Eq, Show)
makeLenses ''Snake

originDirection :: Direction
originDirection = Vec2 0 1

toOriginDirection :: Snake -> Snake
toOriginDirection = set (segments . _head . direction) originDirection

spawn :: (Angle a) => Position -> a Unit -> Snake
spawn pos ang = Snake [(Segment dir pos hRad)] hRad bRad
    where dir  = vecRotation ang originDirection
          hRad = 12
          bRad = 10

extend :: Snake -> Snake
extend s = over segments (++ [newSeg]) s
    where newSeg  = Segment dir pos rad
          dir     = tailSeg ^. direction
          pos     = (tailSeg ^. position) - (fmap (* rad) dir)
          rad     = s ^. bodyRadius
          tailSeg = s ^. segments .to last
          
followSegment :: Segment -> Segment -> Segment
followSegment target seg = over direction (normalize . (+ followVec)) seg
    where followVec = (target ^. position) - (seg ^. position)

applyFriction :: Segment -> Segment -> Distance -> Distance
applyFriction prev curr dist = dist * friction
    where intendedDist = (prev ^. radius) + (curr ^. radius)
          actualDist   = magnitude $ (prev ^. position) - (curr ^. position)
          friction     = actualDist / intendedDist

moveSegment :: Distance -> Segment -> Segment
moveSegment dist seg = set position newPos seg
    where offsetVec = (seg ^. direction) * (pure dist)
          newPos    = (seg ^. position) + offsetVec

cascadeMove :: Distance -> Segment -> Segment -> Segment
cascadeMove dist prev curr = moveSegment frictionedDist followingSegment
    where frictionedDist   = applyFriction prev curr dist
          followingSegment = followSegment prev curr

move :: Distance -> Snake -> Snake
move dist = moveRest . moveHead
    where moveHead = over (segments . _head) (moveSegment dist)
          moveRest = over segments (scanl1 $ cascadeMove dist)

rotate :: (Angle a) => a Unit -> Snake -> Snake
rotate a = over (segments . _head . direction) (vecRotation a)

segmentRight :: Segment -> Position
segmentRight s = (s ^. position) + fmap (* s ^. radius) (perpRight $ s ^. direction)

segmentLeft :: Segment -> Position
segmentLeft s = (s ^. position) + fmap (* s ^. radius) (perpLeft $ s ^. direction)

segmentTop :: Segment -> Position
segmentTop s = (s ^. position) + fmap (* s ^. radius) (s ^. direction)

segmentBottom :: Segment -> Position
segmentBottom s = (s ^. position) - fmap (* s ^. radius) (s ^. direction)

segmentsPath :: [Segment] -> [Point]
segmentsPath segs = 
    map toPair . concat $
        [ [segmentTop (head segs)]
        , map segmentRight segs
        , [segmentBottom (last segs)]
        , reverse . map segmentLeft $ segs
        ]

instance Drawable Snake where
    draw s = color white $ lineLoop path
        where path = segmentsPath (s ^. segments)
