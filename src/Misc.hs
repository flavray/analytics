-- Miscellaneous

module Misc where

import Control.Monad.Trans.Either
import Data.Time.Clock (getCurrentTime, UTCTime)

import qualified Data.ByteString.Char8 as C

-- Get current UTC time
timeNow :: IO C.ByteString
timeNow = C.pack . show <$> getCurrentTime

-- Parse ByteString to UTCTime
parseUTCTime :: C.ByteString -> UTCTime
parseUTCTime = read . C.unpack
