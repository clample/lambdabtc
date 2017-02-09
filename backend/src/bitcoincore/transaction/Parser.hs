module BitcoinCore.Transaction.Parser where

import BitcoinCore.Transaction.Transactions (TxVersion(..), UTXO(..), TxHash(..), TxIndex(..), Transaction(..), TxOutput(..), TxInput(..))
import BitcoinCore.Transaction.Script (Value(..),  ScriptComponent(..), Script(..), getScript)
import General.Util (VarInt(..), roll)

import Data.ByteString (ByteString)
import Crypto.PubKey.ECC.ECDSA (Signature(..))
import Data.Binary (Binary(..))
import Data.Binary.Get (Get, getWord32le, getByteString, getWord64le, getWord8)
import qualified Data.ByteString as BS
import Control.Monad (replicateM)

getTransaction :: Get Transaction
getTransaction = do
  v <- getVersion 
  VarInt inputCount <- get
  inputArray <- replicateM inputCount getInput
  VarInt outputCount <- get
  outputArray <- replicateM outputCount getOutput
  getBlockLockTime
  return Transaction
    { _inputs = inputArray
    , _outputs = outputArray
    , _txVersion = v }

getVersion :: Get TxVersion
getVersion = do
  TxVersion . fromIntegral <$> getWord32le
  
getInput :: Get TxInput
getInput = do
  outPoint <- getOutPoint
  VarInt scriptLength <- get
  script <- getScript scriptLength
  getSequence
  return TxInput
    { _utxo = outPoint
    , _signatureScript = script}

getOutput :: Get TxOutput
getOutput = do
  val <- getTxValue
  VarInt scriptLength <- get
  script <- getScript scriptLength
  return TxOutput
    { _value = val
    , _outputScript = script }

getOutPoint :: Get UTXO
getOutPoint = UTXO
  <$> (TxHash . BS.reverse <$> getByteString 32)
  <*> (TxIndex . fromIntegral <$> getWord32le)

getSequence :: Get ()
getSequence = do
  getWord32le
  return ()

getTxValue :: Get Value
getTxValue =
  Satoshis . fromIntegral <$> getWord64le
  
getBlockLockTime :: Get ()
getBlockLockTime = do
  getWord32le
  return ()

getDerSignature :: Get  Signature
getDerSignature = do
  sequenceCode <- getWord8
  derLength <- fromIntegral <$> getWord8
  getWord8
  xLength <- fromIntegral <$> getWord8
  x <- BS.reverse <$> getByteString xLength
    -- TODO: is the BS.reverse necessary?
  getWord8
  yLength <- fromIntegral <$> getWord8
  y <- BS.reverse <$> getByteString yLength
    -- TODO: is the BS.reverse necessary?
  return $
    Signature (roll x) (roll y)

