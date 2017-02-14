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
import Data.Binary.Get (Get, getWord32le, getByteString, getWord64le, getWord8)
import Data.Binary (Binary(..))
import Control.Monad (replicateM)


data Transaction = Transaction
  { _inputs :: [TxInput]
  , _outputs :: [TxOutput]
  , _txVersion :: TxVersion
  } deriving (Eq, Show)

data TxInput = TxInput
  { _utxo :: UTXO
  , _signatureScript :: Script
  } deriving (Eq, Show)

data TxOutput = TxOutput
  { _value :: Value
  , _outputScript :: Script
  } deriving (Eq, Show)

data UTXO = UTXO
  { _outTxHash :: TxHash
  , _outIndex :: TxIndex
  } deriving (Eq, Show)

data Value = Satoshis Int
  deriving (Eq, Show)

newtype TxVersion = TxVersion Int
  deriving (Eq, Show)

newtype TxHash = TxHash ByteString
  deriving (Eq, Show)

newtype TxIndex = TxIndex Int
  deriving (Eq, Show)

makeLenses ''Transaction
makeLenses ''TxInput
makeLenses ''TxOutput
makeLenses ''UTXO

instance Binary Transaction where
  put = putTransaction
  get = getTransaction

putTransaction :: Transaction -> Put
putTransaction tx = do
  put (tx^.txVersion)
  put . VarInt . fromIntegral . length $ (tx^.inputs)
  mapM_ put (tx^.inputs)
  put . VarInt . fromIntegral . length $ (tx^.outputs)
  mapM_ put (tx^.outputs)
  putBlockLockTime

getTransaction :: Get Transaction
getTransaction = do
  v <- get
  VarInt inputCount <- get
  inputArray <- replicateM inputCount get
  VarInt outputCount <- get
  outputArray <- replicateM outputCount get
  getBlockLockTime
  return Transaction
    { _inputs = inputArray
    , _outputs = outputArray
    , _txVersion = v }

instance Binary TxInput where
  put = putInput
  get = getInput

putInput :: TxInput -> Put
putInput txInput = do
  put (txInput^.utxo)
  putWithLength
    (putScript (txInput^.signatureScript))
  putSequence

getInput :: Get TxInput
getInput = do
  outPoint <- get
  VarInt scriptLength <- get
  script <- getScript scriptLength
  getSequence
  return TxInput
    { _utxo = outPoint
    , _signatureScript = script}

instance Binary TxOutput where
  put = putOutput
  get = getOutput

putOutput :: TxOutput -> Put
putOutput txOutput = do
  put (txOutput^.value)
  putWithLength
    (putScript (txOutput^.outputScript))

getOutput :: Get TxOutput
getOutput = do
  val <- get
  VarInt scriptLength <- get
  script <- getScript scriptLength
  return TxOutput
    { _value = val
    , _outputScript = script }

instance Binary UTXO where
  put = putOutPoint
  get = getOutPoint

putOutPoint :: UTXO -> Put
putOutPoint utxo' = do
  putTxHash (utxo'^.outTxHash)
  let TxIndex i = utxo'^.outIndex
  putWord32le . fromIntegral $ i

getOutPoint :: Get UTXO
getOutPoint = UTXO
  <$> getTxHash
  <*> (TxIndex . fromIntegral <$> getWord32le)

instance Binary Value where
  put = putTxValue
  get = getTxValue

putTxValue :: Value -> Put
putTxValue (Satoshis i) =
  putWord64le . fromIntegral $ i

getTxValue :: Get Value
getTxValue =
  Satoshis . fromIntegral <$> getWord64le

putDerSignature :: Signature -> Put
putDerSignature signature = do
  putWord8 30
  putWithLength $ do
    putWord8 2
    putWithLength (
      putByteString
        -- TODO: Is this being put with correct endian?
      . unroll BE
      . sign_r
      $ signature)
    putWord8 2
    putWithLength (
      putByteString
        -- TODO: Is this being put with correct endian?
      . unroll BE
      . sign_s
      $ signature)

getDerSignature :: Get  Signature
getDerSignature = do
  sequenceCode <- getWord8
  derLength <- fromIntegral <$> getWord8
  getWord8
  xLength <- fromIntegral <$> getWord8
  x <- roll BE <$> getByteString xLength
    -- TODO: is the BS.reverse necessary?
  getWord8
  yLength <- fromIntegral <$> getWord8
  y <- roll BE <$> getByteString yLength
    -- TODO: is the BS.reverse necessary?
  return $
    Signature x y

instance Binary TxVersion where
  put = putTxVersion
  get = getTxVersion

putTxVersion :: TxVersion -> Put
putTxVersion (TxVersion v) =
  putWord32le . fromIntegral $ v

getTxVersion :: Get TxVersion
getTxVersion = do
  TxVersion . fromIntegral <$> getWord32le

defaultVersion :: TxVersion 
defaultVersion = TxVersion 1

instance Binary TxHash where
  put = putTxHash
  get = getTxHash

putTxHash :: TxHash -> Put
putTxHash (TxHash hash) =
  putByteString . BS.reverse $ hash

getTxHash :: Get TxHash
getTxHash = TxHash . BS.reverse <$> getByteString 32

putSequence :: Put
putSequence =
  putWord32le 0xffffffff

getSequence :: Get ()
getSequence = do
  getWord32le
  return ()

putBlockLockTime :: Put
putBlockLockTime =
  putWord32le 0x00000000

getBlockLockTime :: Get ()
getBlockLockTime = do
  getWord32le
  return ()

putSighashAll :: Put
putSighashAll =
  putWord8 1
