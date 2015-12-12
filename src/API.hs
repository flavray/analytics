{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- This module exposes the WAI app so that the main just runs it

module API (app) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson (ToJSON)
import Data.Either (isRight)
import Data.Time.Clock (getCurrentTime, UTCTime)
import GHC.Generics
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant

import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R

data Page = Page { page :: String
                 , connections :: Integer } deriving (Eq, Generic, Show)

data Pages = Pages { pages :: [Page] } deriving (Eq, Generic, Show)

instance ToJSON Page
instance ToJSON Pages

-- Get current UTC time
timeNow :: IO C.ByteString
timeNow = C.pack . show <$> getCurrentTime

-- Main API type
type AnalyticsAPI =
        "analytics" :> QueryParam "name" String
                    :> QueryParam "page" String
                    :> Get '[] ()
  :<|>  "analytics" :> Capture "name" String
                    :> Get '[JSON] Pages

-- Key to the set of pages of the website
redisPages name = C.pack $ "__" ++ name ++ "_pages__"

-- Key to the list of timestamps of the page of the website
redisPage name page = C.pack $ name ++ "_" ++ page

-- Add a connection to the given website's page
-- Insert a new timestamp to the given page
addToWebsite :: R.Connection -> String -> String -> IO Bool
addToWebsite conn name page = do
  time <- timeNow
  R.runRedis conn $ do
    first <- R.sadd (redisPages name) [C.pack page]  -- add page to set of website's pages
    second <- R.lpush (redisPage name page) [time]   -- add timestamp to page
    return $ isRight first && isRight second

-- Get pages and timestamps of a website
websitePages :: R.Connection -> String -> IO Pages
websitePages conn name = do
  pages <- liftIO (
    R.runRedis conn $ do
      pages <- R.smembers $ redisPages name
      case pages of
        Left _ -> return []
        Right pages -> return $ map (fetchPage . C.unpack) pages)
  pages <- sequence pages
  return $ Pages pages
  where fetchPage page = do  -- Get number of timestamps for a given page
          R.runRedis conn $ do
            res <- R.llen $ redisPage name page
            return $ Page page (case res of
                                  Left _ -> 0
                                  Right len -> len)

-- /analytics endpoint handler
analytics :: R.Connection -> Maybe String -> Maybe String -> EitherT ServantErr IO ()
analytics _ Nothing _ = left $ err400 { errBody = "Missing name" }
analytics _ _ Nothing = left $ err400 { errBody = "Missing page" }
analytics conn (Just name) (Just page) = do
  res <- liftIO $ addToWebsite conn name page
  if res then return ()
  else left $ err500 { errBody = "Something went wrong" }

-- /analytics/:name endpoint handler
analyticsName :: R.Connection -> String -> EitherT ServantErr IO Pages
analyticsName conn name = do
  res <- liftIO $ websitePages conn name
  return res

-- Application & server definition

server :: R.Connection -> Server AnalyticsAPI
server conn = analytics conn :<|> analyticsName conn

analyticsAPI :: Proxy AnalyticsAPI
analyticsAPI = Proxy

app :: R.Connection -> Application
app conn = logStdout $ serve analyticsAPI (server conn)
