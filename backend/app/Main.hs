{-# Language OverloadedStrings #-}
module Main where

import LamdaBTC.Server (runApplication)
import General.Persistence (migrateSchema)
import General.Config (developmentConfig)
import Protocol.Server (connectTestnet)
import General.Types (HasPool(..))

import Control.Concurrent (forkIO)
import Control.Lens ((^.))

main :: IO ()
main = do
  config <- developmentConfig
  migrateSchema (config^.pool)
  forkIO $ connectTestnet config
  runApplication config
