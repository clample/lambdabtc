{-# LANGUAGE OverloadedStrings #-}
module TransactionTest where

import BitcoinCore.Transaction.Transactions
import KeyTest()
import TestUtil
import BitcoinCore.Transaction.Script ( Script(..), ScriptComponent(..))
import BitcoinCore.Transaction.Optcodes (OPCODE(..), opcodeTable)
import BitcoinCore.Keys (getPubKey)

import qualified Data.ByteString as BS
import Crypto.PubKey.ECC.ECDSA (Signature(..), PrivateKey(..), PublicKey(..), verify, signWith)
import Crypto.Hash.Algorithms (SHA256(..))
import Data.Binary (Binary(..))
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)

instance Arbitrary Signature where
  arbitrary = do
    x <- arbitrary `suchThat` (> 0)
    y <- arbitrary `suchThat` (> 0)
    return $ Signature x y

instance Arbitrary UTXO where
  arbitrary = do
    hash <- TxHash . BS.pack <$> vectorOf 32 arbitrary
    index <- TxIndex <$> choose (0, 0xffffffff)
    return UTXO
      { _outTxHash = hash
      , _outIndex = index }

instance Arbitrary Value where
  arbitrary = Satoshis <$> arbitrary `suchThat` (> 0)

instance Arbitrary TxOutput where
  arbitrary = do
    value <- arbitrary
    script <- arbitrary
    return TxOutput
      { _value = value
      , _outputScript = script }

instance Arbitrary TxInput where
  arbitrary = do
    utxo' <- arbitrary
    script <- arbitrary
    sequence' <- Sequence <$> arbitrary
    return TxInput
      { _utxo = utxo'
      , _signatureScript = script
      , _sequence = sequence'}

instance Arbitrary TxVersion where
  arbitrary = TxVersion <$> choose (0, 0xffffffff)

instance Arbitrary Transaction where
  arbitrary = do
    inputs' <- arbitrary
    outputs' <- arbitrary
    txVersion' <- arbitrary
    locktime' <- LockTime <$> arbitrary
    return Transaction
      { _inputs = inputs'
      , _outputs = outputs'
      , _txVersion = txVersion'
      , _locktime = locktime'}

instance Arbitrary OPCODE where
  arbitrary = do
    let opcodes = map fst opcodeTable
    elements opcodes

instance Arbitrary ScriptComponent where
  arbitrary = oneof [genTxt, genOp]
    where
      genTxt = do
        txtLength <- choose (1, 75)
        Txt . BS.pack <$> vectorOf txtLength arbitrary
      genOp = OP <$> arbitrary

instance Arbitrary Script where
  arbitrary = Script <$> listOf arbitrary

transactionInvertible = testProperty
  "It should be possible to encode and decode transactions"
  prop_transactionInvertible

prop_transactionInvertible :: Transaction -> Bool
prop_transactionInvertible tx =
  tx == parsedTx
  where
    parsedTx = runGet get (runPut . put $ tx)

signingVerifiable = testProperty
  "We should be able to verify messages that we just signed"
  prop_transactionSigning

prop_transactionSigning :: PrivateKey -> Script -> Transaction -> Bool
prop_transactionSigning privateKey oldInputScript intermediateTransaction =
  verify SHA256 publicKey sig' msg
  where sig = signedHash oldInputScript privateKey intermediateTransaction
        msg = intermediateHash intermediateTransaction oldInputScript
        publicKey = getPubKey privateKey
        sig' = runGet getDerSignature (runPut . putDerSignature $ sig)
