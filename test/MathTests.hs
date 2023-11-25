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
  V4 1 2 3 4 + V4 5 6 7 8 @?= V4 6 8 10 12
