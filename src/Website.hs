{-# LANGUAGE DeriveGeneric #-}

-- Website database-related code

module Website where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON)
import Data.Either (isRight)
import Data.Time.Clock (UTCTime)
import GHC.Generics

import qualified Data.ByteString.Char8 as C
import qualified Database.Redis as R

import Misc

data Page = Page { page :: String
                 , connections :: [UTCTime] } deriving (Eq, Generic, Show)

data Pages = Pages { pages :: [Page] } deriving (Eq, Generic, Show)

instance ToJSON Page
instance ToJSON Pages

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
    second <- R.sadd (redisPage name page) [time]    -- add timestamp to page
    return $ isRight first && isRight second

-- Get pages and timestamps of a website
websitePages :: R.Connection -> String -> IO Pages
websitePages conn name = do
  pages <- liftIO (
    R.runRedis conn $ do
      res <- R.smembers $ redisPages name
      case res of
        Left _ -> return []
        Right pages -> return $ map (fetchPage . C.unpack) pages)
  pages <- sequence pages
  return $ Pages pages
  where fetchPage page = do  -- Get number of timestamps for a given page
          R.runRedis conn $ do
            res <- R.smembers $ redisPage name page
            return $ Page page (case res of
                                  Left _ -> []
                                  Right timestamps -> map parseUTCTime timestamps)
