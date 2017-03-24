{-# LANGUAGE OverloadedStrings #-}
module BitcoinCore.KeysTest where

import TestUtil
import BitcoinCore.Keys
import General.Types (Network(..))
import General.Hash (Hash(..))

import Crypto.PubKey.ECC.ECDSA
  ( PrivateKey(..)
  , PublicKey(..)
  )
import Crypto.PubKey.ECC.Types
  ( ecc_n
  , common_curve
  , getCurveByName
  , CurveName(SEC_p256k1)
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Base58String.Bitcoin
  ( fromText
  , toText
  , b58String
  , toBytes
  )
import qualified Data.Binary as BIN
import Data.ByteString.Base16 (decode)
import Control.Lens ((^.))

instance Arbitrary PrivateKey where
  arbitrary = do
    let max :: Integer
        max = (ecc_n . common_curve . getCurveByName $ SEC_p256k1) - 1
    i <- choose (1, max)
    return $ PrivateKey (getCurveByName SEC_p256k1) i

instance Arbitrary PublicKey where
  arbitrary = do
    privKey <-  arbitrary
    return (getPubKey privKey)

instance Arbitrary PublicKeyRep where
  arbitrary = do
    pubKey <- arbitrary
    format <- elements [Compressed, Uncompressed]
    return $ PublicKeyRep format pubKey

privateKeyInvertible = testProperty
  "Private key should be invertible between hex and private key"
  prop_privateKeyInvertible

prop_privateKeyInvertible :: PrivateKey -> Bool
prop_privateKeyInvertible privKey =
  (deserializePrivateKey . serializePrivateKey) privKey == privKey

publicKeyInvertible = testProperty
  "Public key should be invertible when serialized then deserialized"
  prop_publicKeyInvertible

prop_publicKeyInvertible :: PublicKeyRep -> Bool
prop_publicKeyInvertible pubKeyRep =
  (BIN.decode . BIN.encode) pubKeyRep == pubKeyRep

privateKeyInvertibleWIF = testProperty
  "Private key should be invertible between WIF and private key"
  prop_privateKeyInvertibleWIF

prop_privateKeyInvertibleWIF :: PrivateKey -> Bool
prop_privateKeyInvertibleWIF privKey =
  (getPrivateKeyFromWIF . getWIFPrivateKey) privKey == privKey

uncompressedPubKeyLength = testProperty
  "Uncompressed public key should always be 65 bytes"
  prop_uncompressedPubKeyLength

prop_uncompressedPubKeyLength :: PublicKey -> Bool
prop_uncompressedPubKeyLength pubKey = 
  (BL.length . BIN.encode) (PublicKeyRep Uncompressed pubKey) == 65
  
compressedPubKeyLength = testProperty
  "Compressed public key should always be 33 bytes"
  prop_compressedPubKeyLength

prop_compressedPubKeyLength :: PublicKey -> Bool
prop_compressedPubKeyLength pubKey = 
  (BL.length . BIN.encode) (PublicKeyRep Compressed pubKey) == 33

addressLength = testProperty
  "Address should always be 25 bytes"
  prop_addressLength

prop_addressLength :: PublicKeyRep -> Network -> Bool
prop_addressLength pubKeyRep network =
  addressLength == 25
  where
    address = getAddress pubKeyRep network
    addressLength = BS.length
                    . toBytes
                    . fromText
                    $ address^.addrTxt

pubKeyHashTest = testCase "Hash public key correctly" pubKeyHashAssert

pubKeyHashAssert :: Assertion
pubKeyHashAssert = assertEqual
  "public key hashing should give the expected output"
  (fst . decode $ "010966776006953D5567439E5E39F86A0D273BEE")
  (hash $ hashPubKeyRep testPublicKeyRep)

addressTest = testCase "base58check address" addressAssert

-- See https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
addressAssert :: Assertion
addressAssert = assertEqual
  "We should derive the correct address from a given public key"
  (Address $ toText $ b58String "16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM")
  (getAddress testPublicKeyRep MainNet)

wifPrivateKeyTest = testCase "WIF private key" $ wifPrivateKeyAssert testDataWIFPrivateKey

wifPrivateKeyAssert :: (PrivateKey, WIFPrivateKey) -> Assertion
wifPrivateKeyAssert (input, expected) = assertEqual
  "Private key should be correctly converted to WIF"
  expected
  (getWIFPrivateKey input)
  
testDataWIFPrivateKey =
  ( deserializePrivateKey . fst . decode $ "0C28FCA386C7A227600B2FE50B7CAE11EC86D3BF1FBE471BE89827E19D72AA1D" -- INPUT DATA
  , WIF "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ" -- EXPECTED OUTPUT
  ) 
