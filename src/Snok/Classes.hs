module Snok.Classes where

import FRP.Helm.Graphics (Form)

class Drawable a where
    draw :: a -> Form
