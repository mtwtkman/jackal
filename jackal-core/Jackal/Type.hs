module Jackal.Type where

data EnvironmentError
  = ChannelIdNotFound
  | TokenNotFound

newtype Error
  = EnvironmentError EnvironmentError

type Result a = Either Error a

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
  Roll ffa >>= f = Roll $ fmap (>>= f) ffa