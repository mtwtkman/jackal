module Jackal.Test.TypeTest (tests) where

import Jackal.Type (Free (Roll), FreeT (..), liftF)
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

data TeletypeF a
  = PrintLine !String !a
  | GetLine !(String -> a)

-- instance Functor TeletypeF where
--   fmap f (PrintLine v a) = PrintLine v (f a)
--   fmap f (GetLine g) = GetLine (f . g)

-- instance Applicative TeletypeF where
--
--
-- echo :: Free TeletypeF ()
-- echo = do
--   l <- getLine'
--   printLine l
--
--
-- getLine' :: Free TeletypeF String
-- getLine' = liftF (GetLine id)
--
-- printLine :: String -> Free TeletypeF ()
-- printLine l = liftF (PrintLine l ())
