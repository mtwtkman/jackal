module Main where

import qualified Jackal.Test.FreeTest as FreeTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Jackal test"
    [ FreeTest.tests
    ]
