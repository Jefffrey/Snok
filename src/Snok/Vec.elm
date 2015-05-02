module Snok.Vec where

type alias Unit = Float
type Vec2 = Vec2 Unit Unit

(|+|) : Vec2 -> Vec2 -> Vec2
(Vec2 ax ay) |+| (Vec2 bx by) = Vec2 (ax + bx) (ay + by)

negate : Vec2 -> Vec2
negate (Vec2 x y) = Vec2 (-x) (-y)

(|-|) : Vec2 -> Vec2 -> Vec2
v1 |-| v2 = v1 |+| (negate v2)

(|*) : Vec2 -> Unit -> Vec2
(Vec2 x y) |* k = Vec2 (x * k) (y * k)

(*|) : Unit -> Vec2 -> Vec2
k *| v1 = v1 |* k

(|/) : Vec2 -> Unit -> Vec2
(Vec2 x y) |/ k = Vec2 (x * k) (y * k)

(/|) : Unit -> Vec2 -> Vec2
k /| v1 = v1 |/ k
