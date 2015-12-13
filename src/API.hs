{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- This module exposes the WAI app so that the main just runs it

module API (app) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant

import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R

import Website

-- Main API type
type AnalyticsAPI =
        "analytics" :> QueryParam "name" String
                    :> QueryParam "page" String
                    :> Get '[] ()
  :<|>  "analytics" :> Capture "name" String
                    :> Get '[JSON] Pages

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
