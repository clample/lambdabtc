{-# LANGUAGE OverloadedStrings #-}
module Main where

import Keys
import Data.Base58String.Bitcoin (toBytes, toText)
import Data.Text (unpack)
import Persistence
import Database.Persist.Sqlite (runSqlite, runMigration)
import Data.Text (Text)

main :: IO ()
main = runExample

createAndShowKey = do
  (pubKey, privKey) <- genKeys
  let WIF b58wifPrivKey = getWIFPrivateKey $ getHexPrivateKey privKey
  putStrLn $ unpack  . toText $ b58wifPrivKey
