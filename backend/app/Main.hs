{-# Language OverloadedStrings #-}
module Main where

import LamdaBTC.Server (runApplication)
import General.Persistence (migrateSchema)
import General.Config (developmentConfig, pool)
import Protocol.Server (connectTestnet)
import Control.Concurrent (forkIO)
import Control.Lens ((^.))

main :: IO ()
main = do
  config <- developmentConfig
  migrateSchema (config^.pool)
  forkIO $ connectTestnet config
  runApplication config

