{-# Language OverloadedStrings #-}

import Prelude hiding (length)

import General.Types (Network(..))
import BitcoinCore.Transaction.Script
import BitcoinCore.Transaction.Optcodes
import BitcoinCore.Keys
import BitcoinCore.KeysTest
import BitcoinCore.Transaction.TransactionsTest
import BitcoinCore.Transaction.ScriptTest
import Protocol.PersistenceTest
import Protocol.MessagesTest
import BitcoinCore.BlockHeadersTest
import Protocol.ServerTest
import Protocol.UtilTest
import General.TypesTest
import General.UtilTest
import General.Hash (Hash(..))

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Crypto.PubKey.ECC.ECDSA (PrivateKey(..))
import qualified Data.ByteString.Lazy as BL
import Data.Base58String.Bitcoin (toText, b58String)
import Data.ByteString.Base16 (decode)
import qualified Data.Binary as BIN
import Data.Binary.Put (runPut)

main :: IO ()
main = defaultMain tests

tests =
  [
    testGroup "Script tests" [
      optCodeScriptTest,
      payToPubkeyHashTest
      ],
    testGroup "Key Tests" [
      privateKeyInvertible,
      publicKeyInvertible,
      privateKeyInvertibleWIF,
      uncompressedPubKeyLength,
      compressedPubKeyLength,
      addressLength,
      pubKeyHashTest,
      addressTest,
      wifPrivateKeyTest
      ],
    testGroup "QuickCheck Transaction Tests" [
      transactionInvertible,
      signingVerifiable
      ],
    testGroup "QuickCheck Message Tests" [
      messageInvertible,
      serializingCommand
      ],
    testGroup "QuickCheck BlockHeader Tests" [
      blockHeaderInvertible,
      validHeadersVerify,
      testCase "Check genesis block hash" genesisBlockHash,
      testCase "Check genesis block testnet hash" genesisBlockTestnetHash
      ],
    testGroup "Persistence Tests" [
      persistAndRetrieveBlockHeader,
      persistAndRetrieveTransaction,
      persistAndGetLastBlock,
      getBlockWithIndexAndHash
      ],
    testGroup "Protocol Server Tests" [
      pingAndPong
      ],
    testGroup "Protocol Util Tests" [
      persistentBlockHeaderInvertible
      ],
    testGroup "General.Types Tests" [
      networkInvertible
      ],
    testGroup "General Util Tests" [
      unrollRoll,
      base58CheckInvertible
      ]
  ]
