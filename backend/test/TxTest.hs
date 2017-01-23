module TxTest where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, suchThat)
import Util
import TX
import Crypto.PubKey.ECC.ECDSA (Signature(..))
import Text.Megaparsec
import TX.Parser (parseDerSignature)
import qualified Data.ByteString.Char8 as Char8

instance Arbitrary Signature where
  arbitrary = do
    x <- arbitrary `suchThat` (> 0)
    y <- arbitrary `suchThat` (> 0)
    return $ Signature x y

derSignatureInvertible = testProperty
  "It should be possible to encode and decode a signature through DER"
  prop_derSignatureInvertible

prop_derSignatureInvertible :: Signature -> Bool
prop_derSignatureInvertible sig = 
  case eitherSig of
    Left _ -> False
    Right parsedSig -> sig == parsedSig
  where
    derString = Char8.unpack . derSignature $ sig
    eitherSig = runParser parseDerSignature "" derString
