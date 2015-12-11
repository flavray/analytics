module Main where

import Database.Redis
import Network.Wai.Handler.Warp

import API (app)

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  run 3000 (app conn)
