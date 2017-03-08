{-# LANGUAGE OverloadedStrings #-}
module BitcoinCore.Transaction.TransactionsTest where

import BitcoinCore.Transaction.Transactions
import BitcoinCore.KeysTest ()
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
