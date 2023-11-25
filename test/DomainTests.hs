module DomainTests (domainTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Domain

domainTests :: TestTree
domainTests =
  testGroup
    "Domain Tests"
    [ pointWShouldBe1
    , vectorWShouldBe0
    , floatEqualityShouldBeTrue
    , floatEqualityShouldBeFalse
    ]

pointWShouldBe1 :: TestTree
pointWShouldBe1 =
  testCase "point w = 1" $ V4 1 2 3 1 @?= point 1 2 3

vectorWShouldBe0 :: TestTree
vectorWShouldBe0 =
  testCase "vector w = 0" $ V4 1 2 3 0 @?= vector 1 2 3

floatEqualityShouldBeTrue :: TestTree
floatEqualityShouldBeTrue =
  testCase "float should be eq" $ 0.98198 ~= 0.981981 @?= True

floatEqualityShouldBeFalse :: TestTree
floatEqualityShouldBeFalse =
  testCase "float equality be neq" $ 0.001 ~= 0.0011 @?= False
