module MathTests (mathTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Math

mathTests :: TestTree
mathTests =
  testGroup
    "Math Tests"
    [ pointWShouldBe1
    , vectorWShouldBe0
    , floatEqualityShouldBeTrue
    , floatEqualityShouldBeFalse
    , v4EqualityShouldBeTrue
    , v4EqualityShouldBeFalse
    , v4AdditionShouldBeCorrect
    , subtractTwoPointsShouldBeCorrect 
    , subtractVectorFromPoint
    , subtractTwoVectors
    , subtractVectorFromZeroVector
    ]

pointWShouldBe1 :: TestTree
pointWShouldBe1 =
  testCase "point w = 1" $ V4 1 2 3 1 @?= point 1 2 3

vectorWShouldBe0 :: TestTree
vectorWShouldBe0 =
  testCase "vector w = 0" $ V4 1 2 3 0 @?= vector 1 2 3

floatEqualityShouldBeTrue :: TestTree
floatEqualityShouldBeTrue =
  testCase "float should be eq" $
  (0.98198 :: Float) ~= (0.981981 :: Float) @?= True

floatEqualityShouldBeFalse :: TestTree
floatEqualityShouldBeFalse =
  testCase "float equality be neq" $
  (0.001 :: Float) ~= (0.0011 :: Float) @?= False

v4EqualityShouldBeTrue :: TestTree
v4EqualityShouldBeTrue =
  testCase "vector equality should be eq" $
  V4 0.1 0.2 0.3 0.4 ~= V4 0.1 0.2 0.3 0.4 @?= True

v4EqualityShouldBeFalse :: TestTree
v4EqualityShouldBeFalse =
  testCase "vector equality should be neq" $
  V4 0.1 0.2 0.3 0.4 ~= V4 0.1 0.2 0.30003 0.4 @?= False

v4AdditionShouldBeCorrect :: TestTree
v4AdditionShouldBeCorrect =
  testCase "vector addition should be correct" $
  V4 3 (-2) 5 1 + V4 (-2) 3 1 0 @?= V4 1 1 6 1

subtractTwoPointsShouldBeCorrect :: TestTree
subtractTwoPointsShouldBeCorrect =
  testCase "vector subtraction should be correct" $
  point 3 2 1 - point 5 6 7 @?= vector (-2) (-4) (-6)

subtractVectorFromPoint :: TestTree
subtractVectorFromPoint =
  testCase "subtract vector from point" $
  point 3 2 1 - vector 5 6 7 @?= point (-2) (-4) (-6)

subtractTwoVectors :: TestTree
subtractTwoVectors =
  testCase "subtract two vectors" $
  vector 3 2 1 - vector 5 6 7 @?= vector (-2) (-4) (-6)

subtractVectorFromZeroVector :: TestTree
subtractVectorFromZeroVector =
  testCase "subtract vector from zero vector" $
  (0 :: V4) - vector 1 (-2) 3 @?= vector (-1) 2 (-3)
