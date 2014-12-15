module Snok.Utils where

changeIf :: Bool -> (a -> a) -> a -> a
changeIf b fn a = 
    if b 
        then fn a
        else a

changeUnless :: Bool -> (a -> a) -> a -> a
changeUnless b fn a = changeIf (not b) fn a

inPairs :: (a -> a -> a) -> [a] -> [a]
inPairs _ [] = []
inPairs fn (a:xs) = a : inPairs' fn (a:xs)
    where 
        inPairs' _ [] = []
        inPairs' _ [a] = [a]
        inPairs' fn (a:b:xs) = a : inPairs' fn (fn a b:xs)
