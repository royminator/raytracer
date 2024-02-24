module MathTests (mathTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Math

mathTests :: TestTree
mathTests = testGroup
  "Math Tests"
  [ testCase "point w = 1" $
      V4 1 2 3 1 @?= point 1 2 3
  , testCase "vector w = 0" $
      V4 1 2 3 0 @?= vector 1 2 3
  , testCase "float should be eq" $
      (0.98198 :: Float) ~= (0.981981 :: Float) @?= True
  , testCase "float equality be neq" $
      (0.001 :: Float) ~= (0.0011 :: Float) @?= False
  , testCase "vector equality should be eq" $
      V4 0.1 0.2 0.3 0.4 ~= V4 0.1 0.2 0.3 0.4 @?= True
  , testCase "vector equality should be neq" $
      V4 0.1 0.2 0.3 0.4 ~= V4 0.1 0.2 0.30003 0.4 @?= False
  , testCase "vector addition should be correct" $
      V4 3 (-2) 5 1 + V4 (-2) 3 1 0 @?= V4 1 1 6 1
  , testCase "vector subtraction should be correct" $
      point 3 2 1 - point 5 6 7 @?= vector (-2) (-4) (-6)
  , testCase "subtract vector from point" $
      point 3 2 1 - vector 5 6 7 @?= point (-2) (-4) (-6)
  , testCase "subtract two vectors" $
      vector 3 2 1 - vector 5 6 7 @?= vector (-2) (-4) (-6)
  , testCase "subtract vector from zero vector" $
      (0 :: V4) - vector 1 (-2) 3 @?= vector (-1) 2 (-3)
  ]
