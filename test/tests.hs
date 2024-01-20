module Main where

import qualified Jackal.Test.TypeTest as TypeTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Jackal test"
    [ TypeTest.tests
    ]
