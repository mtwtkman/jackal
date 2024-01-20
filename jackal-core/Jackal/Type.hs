module Jackal.Type where

data EnvironmentError
  = ChannelIdNotFound
  | TokenNotFound

newtype Error
  = EnvironmentError EnvironmentError

type Result a = Either Error a
