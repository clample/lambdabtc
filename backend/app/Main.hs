{-# Language OverloadedStrings #-}
module Main where

import Server (developmentConfig, runApplication)
import Persistence (migrateSchema)
import Protocol.Server (connectTestnet)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  config <- developmentConfig
  migrateSchema config
  forkIO $ connectTestnet config
  runApplication config
