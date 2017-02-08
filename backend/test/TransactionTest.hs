module TransactionTest where

import BitcoinCore.Transaction.Transactions
import KeyTest 
import BitcoinCore.Keys (compressed)
import BitcoinCore.Transaction.Script (Value(..), Script(..), ScriptComponent(..), CompiledScript(..), compile)
import BitcoinCore.Transaction.Optcodes (OPCODE(..), opcodeTable)
import Crypto.PubKey.ECC.ECDSA (Signature(..))
import Text.Megaparsec (runParser, parseMaybe)
import BitcoinCore.Transaction.Parser (parseDerSignature, parseTransaction, ParsedTransaction(..), parseScript)
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import TestUtil

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

instance Arbitrary OPCODE where
  arbitrary = do
    let opcodes = map fst opcodeTable
    elements opcodes

instance Arbitrary ScriptComponent where
  arbitrary = oneof [genTxt, genOp]
    where
      genTxt = do
        txtLength <- arbitrary `suchThat` (\n -> 1 <= n && n <= 75)
        Txt <$> hexBS (2 * txtLength)
      genOp = OP <$> arbitrary

instance Arbitrary Script where
  arbitrary = Script <$> listOf arbitrary
    
derSignatureInvertible = testProperty
  "It should be possible to encode and decode a signature through DER"
  prop_derSignatureInvertible

prop_derSignatureInvertible :: Signature -> Bool
prop_derSignatureInvertible sig = 
  case eitherSig of
    Nothing -> False
    Just parsedSig -> sig == parsedSig
  where
    derString = Char8.unpack . derSignature $ sig
    eitherSig = parseMaybe parseDerSignature derString

transactionInvertible = testProperty
  "It should be possible to encode and decode a transaction"
  prop_transactionInvertible

prop_transactionInvertible :: Transaction -> Bool
prop_transactionInvertible tx =
  case eitherTx of
    Nothing -> False
    Just parsedTx -> compareTransactions parsedTx
  where
    txString = Char8.unpack . signedTransaction $ tx
    eitherTx = parseMaybe parseTransaction txString
    compareTransactions parsedTx =
      (txVersion parsedTx == __version tx) &&
      (and $ zipWith compareInputs (__inputs tx) (inputs parsedTx)) &&
      (and $ zipWith compareOutputs (__outputs tx) (outputs parsedTx))
    compareInputs (utxoIn, privKey) (utxoParsed, compiledScript) = utxoIn == utxoParsed
    compareOutputs txOut (val, compiledScript) =
      value txOut == val

scriptInvertible = testProperty
  "It should be possible to encode and decode a script"
  prop_scriptInvertible

prop_scriptInvertible :: Script -> Bool
prop_scriptInvertible script =
  case eitherScript of
    Nothing -> False
    Just parsedScript -> parsedScript == script
  where
    eitherScript = parseMaybe parseScript scriptString
    scriptString = Char8.unpack scriptBS
    CompiledScript scriptBS = compile script 
