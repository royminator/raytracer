module Math (module Math) where

class ApproxEq a where
  (~=) :: a -> a -> Bool

data V4 = V4 Float Float Float Float
  deriving (Eq, Show)

instance ApproxEq Float where
  (~=) x y = abs (x - y) < eps
    where
      eps = 1e-5

instance Num V4 where
  (+) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    V4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  (-) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    V4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  (*) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    V4 (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
  negate (V4 x y z w) = V4 (-x) (-y) (-z) (-w)
  abs (V4 x y z w) = V4 (abs x) (abs y) (abs z) (abs w)
  signum (V4 x y z w) = V4 (signum x) (signum y) (signum z) (signum w)
  fromInteger x = V4 (fromInteger x) (fromInteger x) (fromInteger x) (fromInteger x)

instance ApproxEq V4 where
  (~=) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    x1 ~= x2 &&
    y1 ~= y2 &&
    z1 ~= z2 &&
    w1 ~= w2

point :: Float -> Float -> Float -> V4
point x y z = V4 x y z 1

vector :: Float -> Float -> Float -> V4
vector x y z = V4 x y z 0

