{-# Language OverloadedStrings #-}
module Main where

import LamdaBTC.Server (developmentConfig, runApplication)
import General.Persistence (migrateSchema)
import Protocol.Server (connectTestnet)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  config <- developmentConfig
  migrateSchema config
  forkIO $ connectTestnet config
  runApplication config

