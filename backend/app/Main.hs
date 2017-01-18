module Main where

import Server (developmentConfig, runApplication)
import Persistence (migrateSchema)


main :: IO ()
main = do
  config <- developmentConfig
  migrateSchema config
  runApplication config
