module Snok.Math where

import Control.Applicative

data Vec2 a = Vec2 a a deriving (Eq, Show)

instance Functor Vec2 where
    fmap fn (Vec2 a b) = Vec2 (fn a) (fn b)

instance Applicative Vec2 where
    pure a = Vec2 a a
    (Vec2 fa fb) <*> (Vec2 a b) = Vec2 (fa a) (fb b)

instance (Num a) => Num (Vec2 a) where
    (+)           = liftA2 (+) 
    (*)           = liftA2 (*)
    abs           = fmap abs
    signum        = fmap signum
    fromInteger x = pure (fromInteger x)
    negate        = fmap negate

magnitude :: Floating a => Vec2 a -> a
magnitude (Vec2 ax ay) = sqrt $ (ax ^^ (2 :: Int)) + (ay ^^ (2 :: Int))

normalize :: Floating a => Vec2 a -> Vec2 a
normalize v = liftA2 (/) v (pure . magnitude $ v)

vecRotation :: (Angle a, Floating b) => a b -> Vec2 b -> Vec2 b
vecRotation ang (Vec2 a b) = 
    let (Radians r) = toRadians ang
        a'          = (a * cos r) - (b * sin r)
        b'          = (a * sin r) + (b * cos r)
    in  Vec2 a' b' 

vecReverse :: Vec2 a -> Vec2 a
vecReverse (Vec2 a b) = (Vec2 b a)

perpLeft :: (Num a) => Vec2 a -> Vec2 a
perpLeft (Vec2 a b) = (Vec2 (-b) a)

perpRight :: (Num a) => Vec2 a -> Vec2 a
perpRight (Vec2 a b) = (Vec2 b (-a))

topVec :: (Num a) => Vec2 a
topVec = Vec2 0 1

rightVec :: (Num a) => Vec2 a
rightVec = Vec2 1 0

bottomVec :: (Num a) => Vec2 a
bottomVec = Vec2 0 (-1)

leftVec :: (Num a) => Vec2 a
leftVec = Vec2 (-1) 0

toPair :: Vec2 a -> (a, a)
toPair (Vec2 a b) = (a, b)

fromPair :: (a, a) -> Vec2 a
fromPair (a, b) = Vec2 a b

data Radians a = Radians a deriving (Eq, Show)
data Degrees a = Degrees a deriving (Eq, Show)

instance Functor Radians where
    fmap fn (Radians a) = Radians (fn a)

instance Functor Degrees where
    fmap fn (Degrees a) = Degrees (fn a)

instance Applicative Radians where
    pure = Radians
    (Radians fn) <*> r = fmap fn r

instance Applicative Degrees where
    pure = Degrees
    (Degrees fn) <*> d = fmap fn d 

class Angle a where
    toRadians   :: Floating b => a b -> Radians b
    toDegrees   :: Floating b => a b -> Degrees b

instance Angle Degrees where
    toRadians (Degrees a) = Radians ((a * pi) / 180)
    toDegrees = id

instance Angle Radians where
    toRadians = id
    toDegrees (Radians a) = Degrees ((a * 180) / pi)

instance (Num a) => Num (Degrees a) where
    (+)           = liftA2 (+) 
    (*)           = liftA2 (*)
    abs           = fmap abs
    signum        = fmap signum
    fromInteger x = Degrees (fromInteger x)
    negate        = fmap negate
