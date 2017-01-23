{-# Language OverloadedStrings #-}
module Main where

import Server (developmentConfig, runApplication)
import Persistence (migrateSchema)
-------------
import Script
import Data.ByteString.Base16 (encode)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)
import Keys
import TX

{--
main :: IO ()
main = do
  config <- developmentConfig
  migrateSchema config
  runApplication config
--}

main = rawExample

showScript :: String
showScript =
  unpack $ decodeUtf8 $ encode bs
  where
    CompiledScript bs = payToPubkeyHash pubKeyRep'

pubKeyRep' = Uncompressed 
  "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"
