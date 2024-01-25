{-# LANGUAGE TupleSections #-}

module Jackal.Test.FreeTest (tests) where

import Jackal.Free (Free (Roll, Return), liftFree)
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

newtype StateF s a = StateF {runStateF :: s -> (a, s)}

instance Functor (StateF s) where
  fmap f (StateF fs) = StateF (\s -> let (a, ns) = fs s in (f a, ns))

instance Applicative (StateF s) where
  pure a = StateF (a,)
  StateF f <*> StateF sa = StateF $ \s ->
    let (a, s') = sa s
        (f', s'') = f s'
     in (f' a, s'')

getF :: StateF s s
getF = StateF $ \s -> (s, s)

putF :: s -> StateF s ()
putF s = StateF $ const ((), s)

type State s = Free (StateF s)

get :: State s s
get = liftFree getF

put :: s -> State s ()
put = liftFree . putF

someComputation :: State Int ()
someComputation = do
  i <- get
  put $ i + 1
  pure ()

runState :: State s a -> s -> (a, s)
runState (Return x) s = (x, s)
runState (Roll f) s =
  let (m, s') = runStateF f s
   in runState m s'

printState :: (Show s, Show a) => State s a -> s -> String
printState (Return x) s = "pure (" <> show x <> "," <> show s <> ")"
printState (Roll m) s =
  let (x, s') = runStateF m s
   in "State change " <> show s <> " -> " <> show s' <> "\n" <> printState x s'
