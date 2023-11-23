module Domain (module Domain) where

data V4 = V4 Float Float Float Float
  deriving (Eq, Show)

point :: Float -> Float -> Float -> V4
point x y z = V4 x y z 1

vector :: Float -> Float -> Float -> V4
vector x y z = V4 x y z 0
