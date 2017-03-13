{-# LANGUAGE OverloadedStrings #-}

module BitcoinCore.Transaction.ScriptTest where

import TestUtil
import BitcoinCore.Transaction.Script
import BitcoinCore.Transaction.Optcodes
import BitcoinCore.Keys

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Base16 (decode)
import Data.Binary.Put (runPut)


optCodeScriptTest = testCase "optcode only script test" optCodeScriptAssert

optCodeScriptAssert :: Assertion
optCodeScriptAssert = assertEqual
  "Simple script should compile correctly"
  (fst . decode $ "76a988ac")
  -- expected output from https://en.bitcoin.it/wiki/Script
  (BL.toStrict . runPut . putScript $ Script [OP OP_DUP, OP OP_HASH160, OP OP_EQUALVERIFY, OP OP_CHECKSIG])

payToPubkeyHashTest = testCase "payToPubkeyHash test" payToPubkeyHashAssert

payToPubkeyHashAssert :: Assertion
payToPubkeyHashAssert = assertEqual
  "payToPubkeyHash should have correct output"
  (fst . decode $ "76a914010966776006953d5567439e5e39f86a0d273bee88ac")
  (BL.toStrict . runPut . putScript . payToPubkeyHash . hashPubKeyRep $ testPublicKeyRep)
