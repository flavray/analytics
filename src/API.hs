{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- This module exposes the WAI app so that the main just runs it

module API (app) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Either (isRight)
import Data.Time.Clock (getCurrentTime)
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant

import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R

-- Get current UTC time
timeNow :: IO C.ByteString
timeNow = C.pack . show <$> getCurrentTime

-- Main API type

type AnalyticsAPI =
      "analytics" :> QueryParam "name" String
                  :> Get '[] ()

-- Add a connection to the given website - Insert a new timestamp
addToWebsite :: R.Connection -> String -> IO Bool
addToWebsite conn name = do
  time <- timeNow
  R.runRedis conn $ do
    res <- R.sadd (C.pack name) [time]
    return $ isRight res

-- /analytics endpoint handler
analytics :: R.Connection -> Maybe String -> EitherT ServantErr IO ()
analytics _ Nothing = left $ err400 { errBody = "Missing name" }
analytics conn (Just name) = do
  res <- liftIO $ addToWebsite conn name
  if res then return ()
  else left $ err500 { errBody = "Bouh!" }

-- Application & server definition

server :: R.Connection -> Server AnalyticsAPI
server conn = analytics conn

analyticsAPI :: Proxy AnalyticsAPI
analyticsAPI = Proxy

app :: R.Connection -> Application
app conn = logStdout $ serve analyticsAPI (server conn)
