{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Keys
import Data.Base58String.Bitcoin (toBytes, toText)
import Server


main :: IO ()
main = runApplication developmentConfig
