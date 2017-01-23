{-# Language OverloadedStrings #-}

import Prelude hiding (length)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Script
import Optcodes
import Keys
import TX
import Crypto.PubKey.ECC.ECDSA (PublicKey(..), PrivateKey(..))
import Crypto.PubKey.ECC.Types (Curve, getCurveByName, Point(..), CurveName(SEC_p256k1))
import Data.ByteString (length)
import Data.Base58String.Bitcoin (Base58String, toText, fromBytes, toBytes, b58String, fromText)
import qualified Data.Text as T
import Data.ByteString.Base16 (decode, encode)
import KeyTest

main :: IO ()
main = defaultMain tests

tests =
  [
    testGroup "Key hashing tests" [
      testCase "Hash public key correctly"
        $ pubKeyHashTest testPublicKeyRep,
      testCase "base58check address"
        $ addressTest testPublicKeyRep,
      testCase "WIF Private key"
        $ testWIFPrivateKey testDataWIFPrivateKey
      ],
    testGroup "Script tests" [
      testCase "optcode only script test"
        optCodeScriptTest,
      testCase "payToPubkeyHash test"
        payToPubkeyHashTest,
      testCase "blockLockTime length test"
        blockLockTimeLengthTest,
      testCase "txVersion length test"
        txVersionLengthTest,
      testCase "txValue test"
        txValueTest,
      testCase "txValue length test"
        txValueLengthTest
      ],
    testGroup "QuickCheck Key Tests" [
      privateKeyInvertibleHex,
      privateKeyInvertibleWIF,
      uncompressedPubKeyLength,
      compressedPubKeyLength,
      addressLength,
      base58CheckInvertible
      ]
  ]

-- This key rep is from https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
testPublicKeyRep :: PublicKeyRep
testPublicKeyRep =
  Uncompressed 
  "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"


pubKeyHashTest :: PublicKeyRep -> Assertion
pubKeyHashTest pubKeyRep = assertEqual
  "public key hashing should give the expected output"
  (stringToHexByteString "010966776006953D5567439E5E39F86A0D273BEE")
  (pubKeyHash pubKeyRep)

addressTest :: PublicKeyRep -> Assertion
addressTest pubKeyRep = assertEqual
  "We should derive the correct address from a given public key"
  (Address $ toText $ b58String "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM")
  (getAddress pubKeyRep)

testWIFPrivateKey :: (PrivateKeyRep, PrivateKeyRep) -> Assertion
testWIFPrivateKey (input, expected) = assertEqual
  "Private key should be correctly converted to WIF"
  expected
  (getWIFPrivateKey input)
 
testDataWIFPrivateKey =
  ( Hex "0C28FCA386C7A227600B2FE50B7CAE11EC86D3BF1FBE471BE89827E19D72AA1D" -- INPUT DATA
  , WIF $ "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ" -- EXPECTED OUTPUT
  ) 

optCodeScriptTest :: Assertion
optCodeScriptTest = assertEqual
  "Simple script should compile correctly"
  (CompiledScript $ fst $ decode "76A988AC")
  -- expected output from https://en.bitcoin.it/wiki/Script
  (compile $ Script [OP OP_DUP, OP OP_HASH160, OP OP_EQUALVERIFY, OP OP_CHECKSIG])

payToPubkeyHashTest :: Assertion
payToPubkeyHashTest = assertEqual
  "payToPubkeyHash should have correct output"
  (CompiledScript $ fst $ decode "76a914010966776006953d5567439e5e39f86a0d273bee88ac")
  (payToPubkeyHash $ Uncompressed 
  "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6")

blockLockTimeLengthTest :: Assertion
blockLockTimeLengthTest = assertEqual
  "block lock time should be 4 bytes long"
  4
  (length blockLockTime)

txVersionLengthTest :: Assertion
txVersionLengthTest = assertEqual
  "tx version should be 4 bytes long"
  4
  (length defaultVersion)

txValueTest :: Assertion
txValueTest = assertEqual
  "txValue should be correctly rendered"
  -- Example taken from http://www.righto.com/2014/02/bitcoins-hard-way-using-raw-bitcoin.html#ref7
  ("6264010000000000")
  (encode $ txValue $ Satoshis 91234)

txValueLengthTest :: Assertion
txValueLengthTest = assertEqual
  "txValue should be 8 bytes long"
  8
  (length $ txValue $ Satoshis 100)
