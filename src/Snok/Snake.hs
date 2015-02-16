{-# LANGUAGE TemplateHaskell #-}

module Snok.Snake
    ( Snake
    , spawn
    , move
    , rotate
    , extend
    , segments
    ) where

import Control.Applicative (pure)
import Snok.Types
import Snok.Classes
import Snok.Math
import Snok.Utils (inPairs)
import Control.Lens
import Prelude hiding (span)
import Debug.Trace 
import qualified FRP.Helm.Graphics as G
import qualified FRP.Helm.Color as C

data Segment =
    Segment {
        _direction :: Direction,
        _position  :: Position
    } deriving (Eq, Show)
makeLenses ''Segment

data Snake =
    Snake { 
        _segments   :: [Segment],
        _span       :: Distance
    } deriving (Eq, Show)
makeLenses ''Snake

originDirection :: Direction
originDirection = Vec2 0 1

toOriginDirection :: Snake -> Snake
toOriginDirection = set (segments._head.direction) originDirection

spawn :: (Angle a) => Position -> a Unit -> Snake
spawn pos ang = extend $ extend $ extend $ Snake [(Segment dir pos)] 10
    where dir = vecRotation ang originDirection

spannedDir :: Snake -> Direction -> Direction
spannedDir s = fmap (* s^.span)

extend :: Snake -> Snake
extend s = over segments (++ [newSeg]) s
    where newSeg  = Segment dir pos
          dir     = tailSeg^.direction
          pos     = tailSeg^.position - spannedDir s dir
          tailSeg = s^.segments.to last

moveSegment :: Distance -> Segment -> Segment
moveSegment dist seg = set position newPos seg
    where newPos    = (seg^.position) + offsetVec
          offsetVec = (seg^.direction) * (pure dist)

followSegment :: Segment -> Segment -> Segment
followSegment target seg = over direction (bisectVec followVec) seg
    where followVec = (target^.position) - (seg^.position)

applyFriction :: Distance -> Segment -> Segment -> Distance -> Distance
applyFriction spn prev curr dist = dist * friction
    where intendedDist = spn * 2
          actualDist   = magnitude $ prev^.position - curr^.position
          friction     = actualDist / intendedDist

cascadeMove :: Distance -> Distance -> Segment -> Segment -> Segment
cascadeMove spn dist prev curr = moveSegment frictionedDist followingSegment
    where frictionedDist   = applyFriction spn prev curr dist
          followingSegment = followSegment prev curr

move :: Distance -> Snake -> Snake
move dist s = traceShow (s^.segments.to (!!1).position) $ moveRest . moveHead $ s
    where moveHead = over (segments._head) (moveSegment dist)
          moveRest = over segments (scanl1 $ cascadeMove spn dist)
          spn      = s^.span

rotate :: (Angle a) => a Unit -> Snake -> Snake
rotate a = over (segments._head.direction) (vecRotation a)

segmentShape :: Unit -> Unit -> Segment -> G.Shape
segmentShape w h seg = 
    G.polygon . map toPair $
        [ r + o
        , r - o
        , l - o
        , l + o
        ]
    where o = fmap (* (h/2)) d
          r = fmap (* (w/2)) . perpRight $ d
          l = fmap (* (w/2)) . perpLeft $ d
          d = seg^.direction

drawSegment :: Int -> (Int, Segment) -> G.Form
drawSegment l (i, seg) = 
    G.move (seg^.position.to toPair) $ G.group
        [ G.filled C.white shp
        , G.outlined (G.solid C.grey) shp
        ]
    where shp = segmentShape (max - ((max - min) * (fromIntegral i / fromIntegral l))) 10 seg
          min = 10
          max = 30

instance Drawable Snake where
    draw s = G.group $ map (drawSegment (length (s^.segments))) $ zip [0..] (s^.segments)
