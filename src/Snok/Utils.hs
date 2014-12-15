module Snok.Utils where

changeIf :: Bool -> (a -> a) -> a -> a
changeIf b fn a = 
    if b 
        then fn a
        else a

changeUnless :: Bool -> (a -> a) -> a -> a
changeUnless b fn a = changeIf (not b) fn a
