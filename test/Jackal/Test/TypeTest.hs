module Jackal.Test.TypeTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Jackal.Type test"
    [ testProperty "sum" $
        \a b -> (a :: Int) + (b :: Int) == a + b
    ]
