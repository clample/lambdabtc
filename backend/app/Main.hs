module Main where

import Keys
import Data.Base58String.Bitcoin (toBytes, toText)
import Data.Text (unpack)

main :: IO ()
main = do
  (pubKey, privKey) <- genKeys
  let WIF b58wifPrivKey = getWIFPrivateKey $ getHexPrivateKey privKey
  putStrLn $ unpack  . toText $ b58wifPrivKey
