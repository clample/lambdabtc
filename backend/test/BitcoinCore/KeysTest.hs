module BitcoinCore.KeysTest where

import TestUtil
import BitcoinCore.Keys
import General.Util
import General.Types (Network(..))
import Crypto.PubKey.ECC.ECDSA (PrivateKey(..), PublicKey(..))
import Crypto.PubKey.ECC.Types (ecc_n, common_curve, getCurveByName, CurveName(SEC_p256k1))
import Crypto.PubKey.ECC.Generate (generateQ)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as Char8
import Data.Base58String.Bitcoin (fromText, toBytes)
import qualified Data.Binary as BIN

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
    (Address b58) = getAddress pubKeyRep network
    addressLength = (BS.length . toBytes . fromText) b58
