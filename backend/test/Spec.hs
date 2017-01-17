{-# Language OverloadedStrings #-}

import Prelude hiding (length)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

import Keys
import Crypto.PubKey.ECC.ECDSA (PublicKey(..), PrivateKey(..))
import Crypto.PubKey.ECC.Types (Curve, getCurveByName, Point(..), CurveName(SEC_p256k1))
import Data.ByteString (length)
import Data.Base58String.Bitcoin (Base58String, toText, fromBytes, toBytes, b58String, fromText)
import qualified Data.Text as T

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
      ]
  ]

-- This key rep is from https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
-- It is not the key rep for testKey
testPublicKeyRep :: PublicKeyRep
testPublicKeyRep =
  Uncompressed $ stringToHexByteString
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
  $ length key

compressedPubKeyLength :: PublicKeyRep -> Assertion
compressedPubKeyLength (Compressed key) = assertEqual
  "Compressed key should have 33 bytes"
  33
  $ length key

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
  ( Hex $ stringToHexByteString "0C28FCA386C7A227600B2FE50B7CAE11EC86D3BF1FBE471BE89827E19D72AA1D" -- INPUT DATA
  , WIF $ "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ" -- EXPECTED OUTPUT
  ) 

