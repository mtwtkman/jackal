module Jackal.Env where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Jackal.Type (EnvironmentError (ChannelIdNotFound, TokenNotFound), Error (EnvironmentError), Result)
import System.Environment (lookupEnv)

data Env = Env
  { envChannelId :: !B.ByteString
  , envToken :: !B.ByteString
  }

fetchEnv :: String -> EnvironmentError -> IO (Result B.ByteString)
fetchEnv s err = do
  e <- lookupEnv s
  case e of
    Nothing -> return $ Left (EnvironmentError err)
    Just v -> return $ Right (T.encodeUtf8 $ T.pack v)

fetchToken :: IO (Result B.ByteString)
fetchToken = fetchEnv "JACKAL_SLACK_API_TOKEN" TokenNotFound

fetchChannelId :: IO (Result B.ByteString)
fetchChannelId = fetchEnv "JACKAL_CHANNEL_ID" ChannelIdNotFound

-- makeEnv :: IO (Result Env)
-- makeEnv = do
--   channelId <- fetchChannelId
--   token <- fetchToken
