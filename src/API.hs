{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- This module exposes the WAI app so that the main just runs it

module API (app) where

import Control.Monad.Trans.Either
import Network.Wai
import Servant

-- Main API type

type AnalyticsAPI = "analytics" :> Get '[JSON] String

-- /analytics endpoint handler

analytics :: EitherT ServantErr IO String
analytics = return $ "Blah"

-- Application & server definition

server :: Server AnalyticsAPI
server = analytics

analyticsAPI :: Proxy AnalyticsAPI
analyticsAPI = Proxy

app :: Application
app = serve analyticsAPI server
