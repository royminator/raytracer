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
    ]

pointWShouldBe1 :: TestTree
pointWShouldBe1 =
  testCase "point w = 1" $ V4 1 2 3 1 @?= point 1 2 3

vectorWShouldBe0 :: TestTree
vectorWShouldBe0 =
  testCase "vector w = 0" $ V4 1 2 3 0 @?= vector 1 2 3
