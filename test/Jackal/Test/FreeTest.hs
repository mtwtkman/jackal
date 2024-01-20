module Jackal.Test.FreeTest (tests) where

import Jackal.Free (Free (Roll), liftFree)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty)

tests :: TestTree
tests =
  testGroup
    "Jackal.Type test"
    [ prop_Free
    ]

prop_Free :: TestTree
prop_Free =
  testGroup
    "Free monad"
    []

newtype Context a = Context {runContext :: a}

instance Functor Context where
  fmap f (Context a) = Context (f a)

instance Applicative Context where
  pure = Context
  (Context f) <*> (Context a) = Context (f a)

getContext :: String -> Free Context String
getContext = liftFree . Context

readContext :: Free Context ()
readContext = do
  runFr
