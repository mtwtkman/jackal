module Jackal.Free where

data Free f a = Return !a | Roll !(f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Return a) = Return (f a)
  fmap f (Roll ffa) = Roll (fmap (fmap f) ffa)

instance (Applicative f) => Applicative (Free f) where
  pure = Return
  Return f <*> Return a = Return (f a)
  Return f <*> r@(Roll _) = fmap f r
  Roll ff <*> Return a = Roll (fmap (fmap (\f -> f a)) ff)
  Roll ff <*> r@(Roll _) = Roll (fmap (<*> r) ff)

instance (Functor f, Applicative f) => Monad (Free f) where
  return = pure
  Return a >>= f = f a
  Roll ff >>= f = Roll ((>>= f) <$> ff)

liftFree :: (Functor f) => f a -> Free f a
liftFree fa = Roll (fmap Return fa)

foldFree :: (Functor f) => (f a -> a) -> Free f a -> a
foldFree _ (Return a) = a
foldFree f (Roll ff) = f (fmap (foldFree f) ff)

runFree :: (Monad m) => Free m a -> m a
runFree (Return a) = return a
runFree (Roll ff) = ff >>= runFree

newtype ReaderF r f a = ReaderF {runReaderF :: r -> f a}

instance (Functor f) => Functor (ReaderF r f) where
  fmap f (ReaderF rf) = ReaderF $ \r -> fmap f (rf r)

instance (Applicative f) => Applicative (ReaderF r f) where
  pure a = ReaderF $ const (pure a)
  ReaderF f <*> ReaderF run = ReaderF $ \r -> f r <*> run r

type Reader r f = Free (ReaderF r f)
