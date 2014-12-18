{-# LANGUAGE TemplateHaskell #-}

module Snok.Input where

import Control.Lens hiding ((<~))
import Snok.Types (Time)
import FRP.Helm.Signal (Signal, (<~), (~~))
import qualified FRP.Helm.Keyboard as K
import qualified FRP.Helm.Time as T

data Input
    = Input { _delta        :: Time
            , _pressedKeys  :: [K.Key] }
makeLenses ''Input

inputSignal :: Signal Input
inputSignal = makeInput <~ (T.fps 70) ~~ K.keysDown
    where makeInput md kd = 
              if md < 1000
                  then Input (md / T.second) kd
                  else Input 0 kd
