module KeyTest where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose)
import Keys
import Crypto.PubKey.ECC.ECDSA (PrivateKey(..), PublicKey(..))
import Crypto.PubKey.ECC.Types (ecc_n, common_curve, getCurveByName, CurveName(SEC_p256k1))
import Crypto.PubKey.ECC.Generate (generateQ)
import qualified Data.ByteString as BS

instance Arbitrary PrivateKey where
  arbitrary = do
    let max :: Integer
        max = (ecc_n . common_curve . getCurveByName $ SEC_p256k1) - 1
    i <- choose (1, max)
    return $ PrivateKey (getCurveByName SEC_p256k1) i

instance Arbitrary PublicKey where
  arbitrary = do
    privKey <-  arbitrary
    let curve = getCurveByName SEC_p256k1
        pubPoint = generateQ (getCurveByName SEC_p256k1) (private_d privKey)
    return (PublicKey curve pubPoint)


privateKeyInvertible = testProperty
  "Private key should be invertible between hex and private key"
  prop_privateKeyInvertible

prop_privateKeyInvertible :: PrivateKey -> Bool
prop_privateKeyInvertible privKey = (getPrivateKeyFromHex . getHexPrivateKey) privKey == privKey

uncompressedPubKeyLength = testProperty
  "Uncompressed public key should always be 65 bytes"
  prop_uncompressedPubKeyLength

prop_uncompressedPubKeyLength :: PublicKey -> Bool
prop_uncompressedPubKeyLength pubKey = 
  BS.length (textToHexByteString key) == 65
  where
    (Uncompressed key) = uncompressed pubKey

compressedPubKeyLength = testProperty
  "Compressed public key should always be 33 bytes"
  prop_compressedPubKeyLength

prop_compressedPubKeyLength :: PublicKey -> Bool
prop_compressedPubKeyLength pubKey = 
  BS.length (textToHexByteString key) == 33
  where
    (Compressed key) = compressed pubKey


