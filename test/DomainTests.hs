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
  testCase "point w = 0" $ do
    let expected = V4 1 2 3 1
    let actual = point 1 2 3
    expected @?= actual

vectorWShouldBe0 :: TestTree
vectorWShouldBe0 =
  testCase "vector w = 0" $ do
    let expected = V4 1 2 3 0
    let actual = vector 1 2 3
    expected @?= actual
