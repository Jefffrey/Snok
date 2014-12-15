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
import Snok.Math (Vec2(..), Angle(..), normalize, vecRotation, toPair)
import Snok.Utils (inPairs)
import Control.Lens
import Debug.Trace (traceShowId)

data Segment =
    Segment { _direction :: Direction 
            , _position  :: Position }
    deriving (Eq, Show)
makeLenses ''Segment

data Snake = Snake { _segments :: [Segment] 
                   , _segmentDistance :: Unit 
                   } deriving (Eq, Show)
makeLenses ''Snake

originDirection :: Direction
originDirection = Vec2 0 1

toOriginDirection :: Snake -> Snake
toOriginDirection = (segments . _head . direction) .~ originDirection

spawn :: (Angle a) => Position -> a Unit -> Snake
spawn p a = 
    let nd = vecRotation a originDirection
    in  Snake [(Segment nd p)] 10

extend :: Snake -> Snake
extend s =
    let (Just t) = s ^. segments ^? _last
        sd = s ^. segmentDistance
        np = liftA2 (-) (t ^. position) (fmap (* sd) (t ^. direction))
        ns = Segment (t ^. direction) np
    in  over segments (++ [ns]) s

followSegment :: Segment -> Segment -> Segment
followSegment t s =
    let tp = t ^. position
        sp = s ^. position
        tv = tp - sp
    in  over direction (\a -> normalize (a + tv)) s

moveSegment :: Distance -> Segment -> Segment
moveSegment d sg =
    let op = sg ^. position
        ov = liftA2 (*) (sg ^. direction) (pure d)
        np = liftA2 (+) op ov
    in  position .~ np $ sg

move :: Distance -> Snake -> Snake
move d = over segments (map (moveSegment d) . scanl1 followSegment)

rotate :: (Angle a) => a Unit -> Snake -> Snake
rotate a = over (segments . _head . direction) (vecRotation a)

instance Drawable Segment where
    draw sg = 
        let p = sg ^. position
            pe = p + (fmap (5*) (sg ^. direction))
            c = (uncurry translate) (toPair p) $ color red $ circleSolid 5
            l = color blue $ line [toPair p, toPair pe]
        in  pictures [c, l]

instance Drawable Snake where
    draw s = pictures $ map draw $ s ^. segments
