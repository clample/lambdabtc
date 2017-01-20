{-# Language OverloadedStrings #-}

import Prelude hiding (length)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Script
import Optcodes
import Keys
import Transaction
import Crypto.PubKey.ECC.ECDSA (PublicKey(..), PrivateKey(..))
import Crypto.PubKey.ECC.Types (Curve, getCurveByName, Point(..), CurveName(SEC_p256k1))
import Data.ByteString (length)
import Data.Base58String.Bitcoin (Base58String, toText, fromBytes, toBytes, b58String, fromText)
import qualified Data.Text as T
import Data.ByteString.Base16 (decode, encode)

main :: IO ()
main = defaultMain tests

tests =
  [
    -- Helps to make sure the encoding is correct
    testGroup "length tests" [
      testCase "Uncompressed public key length"
        $ uncompressedPubKeyLength $ uncompressed testKey,
      testCase "Compressed public key length"
        $ compressedPubKeyLength $ compressed testKey,
      testCase "Address length"
        $ addressLengthTest $ compressed testKey
      ],
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
      ]
  ]

-- This key rep is from https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
-- It is not the key rep for testKey
testPublicKeyRep :: PublicKeyRep
testPublicKeyRep =
  Uncompressed 
  "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"


testKey :: PublicKey
testKey = PublicKey
  (getCurveByName SEC_p256k1)
  (Point
    55066263022277343669578718895168534326250603453777594175500187360389116729240
    32670510020758816978083085130507043184471273380659243275938904335757337482424)

pubKeyHashTest :: PublicKeyRep -> Assertion
pubKeyHashTest pubKeyRep = assertEqual
  "public key hashing should give the expected output"
  (stringToHexByteString "010966776006953D5567439E5E39F86A0D273BEE")
  (pubKeyHash pubKeyRep)

uncompressedPubKeyLength :: PublicKeyRep -> Assertion
uncompressedPubKeyLength (Uncompressed key) = assertEqual
  "Uncompressed key should have 65 bytes"
  65
  $ length (textToHexByteString key)

compressedPubKeyLength :: PublicKeyRep -> Assertion
compressedPubKeyLength (Compressed key) = assertEqual
  "Compressed key should have 33 bytes"
  33
  $ length (textToHexByteString key)

addressTest :: PublicKeyRep -> Assertion
addressTest pubKeyRep = assertEqual
  "We should derive the correct address from a given public key"
  (Address $ toText $ b58String "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM")
  (getAddress pubKeyRep)

addressLengthTest :: PublicKeyRep -> Assertion
addressLengthTest pubKeyRep = assertEqual
  "Address should have 25 bytes"
  25
  addressLength
  where
    Address b58 = getAddress pubKeyRep
    addressLength = length $ toBytes . fromText $ b58

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
  (length txVersion)

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
