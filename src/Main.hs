module Main where

import Control.Exception (catch)
import Snok.Game (Except(..), Game, update, start, render)
import Snok.Input (inputSignal)
import FRP.Helm.Graphics (Element)
import FRP.Helm (Signal, (<~), (~~), foldp, run, defaultConfig)
import qualified FRP.Helm.Window as W
import qualified FRP.Helm.Time as T
import qualified FRP.Helm.Graphics as G
import qualified FRP.Helm.Color as C
import FRP.Helm.Text (asText)

logic :: Game -> Signal Game
logic g = foldp update g inputSignal

view :: Game -> Signal Element
view g = render <~ W.dimensions ~~ logic g

main :: IO ()
main = do
    game <- start
    catch
        (run defaultConfig (view game))
        (\EndGame -> return ())
