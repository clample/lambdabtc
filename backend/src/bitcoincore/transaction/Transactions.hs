{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module BitcoinCore.Transaction.Transactions where

import General.Util
import BitcoinCore.Transaction.Script

import Prelude hiding (concat, reverse, sequence)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.PubKey.ECC.ECDSA (signWith, Signature(..), PrivateKey(..))
import Control.Lens (makeLenses, (^.))
import Data.Binary.Put (Put, putWord8, putWord32le, putWord64le, putByteString)
import Data.Binary (Binary(..))

newtype TxVersion = TxVersion Int
  deriving (Eq, Show)

newtype TxHash = TxHash ByteString
  deriving (Eq, Show)

newtype TxIndex = TxIndex Int
  deriving (Eq, Show)


data TxOutput = TxOutput
  { _value :: Value
  , _outputScript :: Script
  } deriving (Eq, Show)

makeLenses ''TxOutput

data UTXO = UTXO
  { _outTxHash :: TxHash
  , _outIndex :: TxIndex
  } deriving (Eq, Show)

makeLenses ''UTXO

data TxInput = TxInput
  { _utxo :: UTXO
  , _signatureScript :: Script
  } deriving (Eq, Show)

makeLenses ''TxInput

data Transaction = Transaction
  { _inputs :: [TxInput]
  , _outputs :: [TxOutput]
  , _txVersion :: TxVersion
  } deriving (Eq, Show)

makeLenses ''Transaction

putBlockLockTime :: Put
putBlockLockTime =
  putWord32le 0x00000000

defaultVersion :: TxVersion 
defaultVersion = TxVersion 1

putOutPoint :: UTXO -> Put
putOutPoint utxo = do
  putTxHash (utxo^.outTxHash)
  let TxIndex i = utxo^.outIndex
  putWord32le . fromIntegral $ i

putTxHash :: TxHash -> Put
putTxHash (TxHash hash) =
  putByteString . BS.reverse $ hash

putTxValue :: Value -> Put
putTxValue (Satoshis i) =
  putWord64le . fromIntegral $ i

putTxVersion :: TxVersion -> Put
putTxVersion (TxVersion v) =
  putWord32le . fromIntegral $ v

putSequence :: Put
putSequence =
  putWord32le 0xffffffff

putTxInput :: TxInput -> Put
putTxInput txInput = do
  putOutPoint (txInput^.utxo)
  putWithLength
    (putScript (txInput^.signatureScript))
  putSequence

putTxOutput :: TxOutput -> Put
putTxOutput txOutput = do
  putTxValue (txOutput^.value)
  putWithLength
    (putScript (txOutput^.outputScript))

putTransaction :: Transaction -> Put
putTransaction (Transaction inputs outputs txVersion) = do
  putTxVersion txVersion
  put . VarInt . fromIntegral . length $ inputs
  mapM_ putTxInput inputs
  put . VarInt . fromIntegral . length $ outputs
  mapM_ putTxOutput outputs
  putBlockLockTime

putDerSignature :: Signature -> Put
putDerSignature signature = do
  putWord8 30
  putWithLength $ do
    putWord8 2
    putWithLength (
      putByteString
      . BS.reverse
        -- TODO: Is this being put with correct endian?
      . unroll
      . sign_r
      $ signature)
    putWord8 2
    putWithLength (
      putByteString
      . BS.reverse
        -- TODO: Is this being put with correct endian?
      . unroll
      . sign_s
      $ signature)

putSighashAll :: Put
putSighashAll =
  putWord8 1


{--
signedTransaction :: Transaction -> Put
signedTransaction tx@(Transaction _ _ txVersion) =
  putTransaction tx (\(_, privKey) -> scriptSig fillerTransaction privKey)
  where
    fillerTransaction = do
      putTransaction tx (\(utxo, _) -> getUtxoScript utxo)
      putTxVersion defaultVersion
--}


{--
getUtxoScript :: UTXO -> CompiledScript -- Returns the output script from the given UTXO
getUtxoScript _ = CompiledScript "76a914010966776006953d5567439e5e39f86a0d273bee88ac"
--}


{--
-- TODO: Move this to Script.hs?
scriptSig :: ByteString -> PrivateKey -> CompiledScript
scriptSig rawTx privKey =
  CompiledScript $ BS.concat
  [ derSigHashLength
  , signedHashDER
  , publicKeyBSLength
  , publicKeyBS
  ]
  where
  rawTxHash = pack . show . hashWith SHA256 . hashWith SHA256 $ rawTx
  signedHash = fromJust $ signWith 100 privKey SHA256 rawTxHash
    -- TODO: CHANGE THIS!
    -- signWith should use a random number, not a hardcoded 100
    -- fromJust will cause runtime errors
  signedHashDER = derSignature signedHash `BS.append` sighashAll
  derSigHashLength = payloadLength signedHashDER
  Compressed publicKeyText = compressed . getPubKey $ privKey
  publicKeyBS = T.encodeUtf8 publicKeyText
  publicKeyBSLength = payloadLength publicKeyBS
--}
