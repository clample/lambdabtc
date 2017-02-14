module KeyTest where

import TestUtil
import BitcoinCore.Keys
import General.Util
import General.Types (Network(..))
import Crypto.PubKey.ECC.ECDSA (PrivateKey(..), PublicKey(..))
import Crypto.PubKey.ECC.Types (ecc_n, common_curve, getCurveByName, CurveName(SEC_p256k1))
import Crypto.PubKey.ECC.Generate (generateQ)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Base58String.Bitcoin (fromText, toBytes)

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

instance Arbitrary Prefix where
  arbitrary = do
    char <- arbitrary
    return (prefix $ Char8.pack [char])
    
instance Arbitrary Payload where
  arbitrary = do
    str <- arbitrary
    return (Payload $ Char8.pack str)


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
prop_publicKeyInvertible pubKeyRep@(PublicKeyRep format pubKey) =
  pubKey == deserializedPubKey
  where
    deserializedPubKey =
      case (deserializePublicKeyRep . serializePublicKeyRep) pubKeyRep of
        Left err -> error err
        Right dpk -> dpk

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
  (BS.length . serializePublicKeyRep) (PublicKeyRep Uncompressed pubKey) == 65
  
compressedPubKeyLength = testProperty
  "Compressed public key should always be 33 bytes"
  prop_compressedPubKeyLength

prop_compressedPubKeyLength :: PublicKey -> Bool
prop_compressedPubKeyLength pubKey = 
  (BS.length . serializePublicKeyRep) (PublicKeyRep Compressed pubKey) == 33

addressLength = testProperty
  "Address should always be 25 bytes"
  prop_addressLength

prop_addressLength :: PublicKeyRep -> Network -> Bool
prop_addressLength pubKeyRep network =
  addressLength == 25
  where
    (Address b58) = getAddress pubKeyRep network
    addressLength = (BS.length . toBytes . fromText) b58

base58CheckInvertible = testProperty
  "We should be able to convert to and from base 58 check"
  prop_base58CheckInvertible

prop_base58CheckInvertible :: Prefix -> Payload -> Bool
prop_base58CheckInvertible prefix payload  =
  (prefix == decodedPrefix) && (payload == decodedPayload)
  where
    b58 = encodeBase58Check prefix payload
    (decodedPrefix, decodedPayload, _) = decodeBase58Check b58
  
