{-# Language OverloadedStrings #-}
module Main where

import LamdaBTC.Server (runApplication)
import General.Persistence (migrateSchema)
import General.Config (developmentConfig)
import Protocol.Server (connectTestnet)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  config <- developmentConfig
  migrateSchema config
  forkIO $ connectTestnet config
  runApplication config

