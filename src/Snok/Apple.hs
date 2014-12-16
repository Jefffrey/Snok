{-# LANGUAGE TemplateHaskell #-}

module Snok.Apple
    ( Apple
    , spawn
    ) where

import Snok.Types
import Control.Lens
import Graphics.Gloss
import Snok.Math

data Apple
    = Apple 
        { _position :: Position
        , _radius   :: Dimension
        } deriving (Eq, Show)
makeLenses ''Apple

spawn :: Position -> Apple
spawn pos = Apple pos 8

instance Drawable Apple where
    draw app = color red $ circle rad
        where rad = app ^. radius
