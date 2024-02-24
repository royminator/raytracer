module Math (module Math) where

class ApproxEq a where
  (~=) :: a -> a -> Bool

data V4 = V4
  { x :: Float,
    y :: Float,
    z :: Float,
    w :: Float
  }
  deriving (Eq, Show)

instance ApproxEq Float where
  (~=) a b = abs (a - b) < eps
    where
      eps = 1e-5

instance Num V4 where
  (+) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    V4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  (-) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    V4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  (*) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    V4 (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
  negate (V4 x_ y_ z_ w_) = V4 (-x_) (-y_) (-z_) (-w_)
  abs (V4 x_ y_ z_ w_) = V4 (abs x_) (abs y_) (abs z_) (abs w_)
  signum (V4 x_ y_ z_ w_) = V4 (signum x_) (signum y_) (signum z_) (signum w_)
  fromInteger x_ = V4 (fromInteger x_) (fromInteger x_) (fromInteger x_) (fromInteger x_)

instance ApproxEq V4 where
  (~=) (V4 x1 y1 z1 w1) (V4 x2 y2 z2 w2) =
    x1 ~= x2
      && y1 ~= y2
      && z1 ~= z2
      && w1 ~= w2

point :: Float -> Float -> Float -> V4
point x_ y_ z_ = V4 x_ y_ z_ 1

vector :: Float -> Float -> Float -> V4
vector x_ y_ z_ = V4 x_ y_ z_ 0
