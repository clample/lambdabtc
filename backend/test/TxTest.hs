module TxTest where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, suchThat, vectorOf, elements, Gen)
import TX
import KeyTest 
import Keys (compressed)
import Script (Value(..))
import Crypto.PubKey.ECC.ECDSA (Signature(..))
import Text.Megaparsec (runParser)
import TX.Parser (parseDerSignature, parseTransaction, ParsedTransaction(..))
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)

instance Arbitrary Signature where
  arbitrary = do
    x <- arbitrary `suchThat` (> 0)
    y <- arbitrary `suchThat` (> 0)
    return $ Signature x y

instance Arbitrary UTXO where
  arbitrary = do
    hash <- hexBS 64
    index <- choose (0, 9)
    return $ UTXO hash index

hexBS :: Int -> Gen ByteString
hexBS i = Char8.pack <$> vectorOf i hexChars
  where
    hexChars = elements ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']

instance Arbitrary Value where
  arbitrary = Satoshis <$> arbitrary `suchThat` (> 0)

instance Arbitrary TxOutput where
  arbitrary = do
    value <- arbitrary
    pubKeyRep <- compressed <$> arbitrary
    return $ TxOutput value pubKeyRep

instance Arbitrary Transaction where
  arbitrary = do
    utxo <- arbitrary
    privKey <- arbitrary
    txOutput <- arbitrary
    return $ Transaction [(utxo, privKey)] [txOutput] defaultVersion
    
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

transactionInvertible = testProperty
  "It should be possible to encode and decode a transaction"
  prop_transactionInvertible

prop_transactionInvertible :: Transaction -> Bool
prop_transactionInvertible tx =
  case eitherTx of
    Left _ -> False
    Right parsedTx -> compareTransactions parsedTx
  where
    txString = Char8.unpack . signedTransaction $ tx
    eitherTx = runParser parseTransaction "" txString
    compareTransactions parsedTx =
      (version parsedTx == __version tx) &&
      (and $ zipWith compareInputs (__inputs tx) (inputs parsedTx)) &&
      (and $ zipWith compareOutputs (__outputs tx) (outputs parsedTx))
    compareInputs (utxoIn, privKey) (utxoParsed, compiledScript) = utxoIn == utxoParsed
    compareOutputs txOut (val, compiledScript) =
      (value txOut == val)
