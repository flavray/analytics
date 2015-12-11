module Main where

import Network.Wai.Handler.Warp

import API (app)

main :: IO ()
main = do
  run 3000 app
