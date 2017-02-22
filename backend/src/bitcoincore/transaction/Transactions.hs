{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module BitcoinCore.Transaction.Transactions where

import General.Util
import BitcoinCore.Transaction.Script
import BitcoinCore.Keys (serializePublicKeyRep, PublicKeyRep(..), PubKeyFormat(..))

import Prelude hiding (concat, reverse, sequence)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Crypto.PubKey.ECC.ECDSA (signWith, Signature(..), PrivateKey(..), PublicKey(..))
import Crypto.Hash.Algorithms (SHA256(..))
import Control.Lens (makeLenses, (^.), to, mapped, set)
import Data.Binary.Put (Put, putWord8, putWord32le, putWord64le, putByteString, runPut)
import Data.Binary.Get (Get, getWord32le, getByteString, getWord64le, getWord8, runGet)
import Data.Binary (Binary(..), Word32)
import Control.Monad (replicateM)
import Data.Bits ((.&.))


data Transaction = Transaction
  { _inputs :: [TxInput]
  , _outputs :: [TxOutput]
  , _txVersion :: TxVersion
  , _locktime :: LockTime
  } deriving (Eq, Show)

data TxInput = TxInput
  { _utxo :: UTXO
  , _signatureScript :: Script
  , _sequence :: Sequence
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
  deriving (Eq)

instance Show TxHash where
  show (TxHash bs) = "TxHash " ++ (show . encode $ bs)

newtype TxIndex = TxIndex Int
  deriving (Eq, Show)

newtype Sequence = Sequence Word32
  deriving (Eq, Show)

newtype LockTime = LockTime Word32
  deriving (Eq, Show)

makeLenses ''Transaction
makeLenses ''TxInput
makeLenses ''TxOutput
makeLenses ''UTXO

outputScripts :: Transaction -> [Script]
outputScripts transaction = map (^.outputScript) (transaction^.outputs)

signedTransaction :: UTXO -> Script -> (PublicKey, PrivateKey) -> [TxOutput] -> Transaction
signedTransaction utxo' oldInputScript keys outputs'  = 
  (set (inputs.mapped.signatureScript) newInputScript transaction)
  where
    fillerTransactionBS = BL.toStrict . runPut $ do
      put (set (inputs.mapped.signatureScript) oldInputScript transaction)
      putWord32le sighashAll
    hash = doubleSHA fillerTransactionBS

    -- TODO: k should be a random number, not a hardcoded 100!
    signedHash = case signWith 100 (snd keys) SHA256 hash of
                   Nothing -> error "Unable to sign hash"
                   Just sig -> sig
    newInputScript = scriptSig signedHash (fst keys)

    -- TODO: is there a cleaner way to do this?  
    transaction = Transaction
      {_inputs = [TxInput { _utxo = utxo'
                          , _sequence = defaultSequence}]
      , _outputs = outputs'
      , _txVersion = TxVersion 1
      , _locktime = defaultLockTime}

------------------- For testing
{--
utxo' = UTXO
  { _outTxHash = TxHash . BS.reverse . fst . decode $ "eccf7e3034189b851985d871f91384b8ee357cd47c3024736e5676eb2debb3f2"
  , _outIndex = TxIndex 1
  }

outputs' = [TxOutput
             { _value = Satoshis 99900000
             , _outputScript = outputScript'}]

oldInputScript = runGet (getScript 25) ( BL.fromChunks [fst . decode $ "76a914010966776006953d5567439e5e39f86a0d273bee88ac"]) :: Script

outputScript' = runGet (getScript 25) ( BL.fromChunks [fst . decode $ "76a914097072524438d003d23a2f23edb65aae1bb3e46988ac"]) :: Script
--}
--------------------

sighashAll :: Word32
sighashAll = 0x00000001

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
  put $ tx^.locktime

getTransaction :: Get Transaction
getTransaction = do
  v <- get
  VarInt inputCount <- get
  inputArray <- replicateM inputCount get
  VarInt outputCount <- get
  outputArray <- replicateM outputCount get
  locktime' <- get
  return Transaction
    { _inputs = inputArray
    , _outputs = outputArray
    , _txVersion = v
    , _locktime = locktime'}

instance Binary TxInput where
  put = putInput
  get = getInput

putInput :: TxInput -> Put
putInput txInput = do
  put (txInput^.utxo)
  putWithLength
    (putScript (txInput^.signatureScript))
  put $ txInput^.sequence

getInput :: Get TxInput
getInput = do
  outPoint <- get
  VarInt scriptLength <- get
  script <- getScript scriptLength
  sequence' <- get
  return TxInput
    { _utxo = outPoint
    , _signatureScript = script
    , _sequence = sequence' }

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

scriptSig :: Signature -> PublicKey -> Script
scriptSig signature pubKey = Script [Txt der, Txt compressedPubkey]
  where der = (BL.toStrict . runPut $ putDerSignature signature >> putWord8 1)
        compressedPubkey = serializePublicKeyRep
          $ PublicKeyRep Compressed pubKey
          -- TODO: This scriptSig will only be valid for
          -- pay to pub key hash scripts where the compressed pub key is hashed
          -- make sure that I'm making addresses using compressed pubkeys also
          
-- See https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki
-- for a description of requiered der format
putDerSignature :: Signature -> Put
putDerSignature signature = do
  putWord8 0x30
  putWithLength $ do
    putWord8 0x02
    putWithLength
      (putDERInt . sign_r $ signature)
    putWord8 0x02
    putWithLength
      (putDERInt . getLowS . sign_s $ signature)
  where
    putDERInt int = do
      let intBS = unroll BE int
          headByte = BS.head intBS
      if (headByte .&. 0x80 == 0x80)
        then putByteString $ 0x00 `BS.cons` intBS
        else putByteString intBS                    

-- Multiple s values can yield the same signature
-- to prevent transaction malleability, s values are required to use the "low s" value
-- See: https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
getLowS :: Integer -> Integer
getLowS s = if s <= maxS
            then s
            else constant - s
  where maxS = 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0
        constant =  0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

getDerSignature :: Get Signature
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

hashTransaction :: Transaction -> TxHash
hashTransaction transaction = TxHash $
  BS.reverse . doubleSHA . BL.toStrict . runPut $ put transaction

instance Binary TxHash where
  put = putTxHash
  get = getTxHash

putTxHash :: TxHash -> Put
putTxHash (TxHash hash) =
  putByteString . BS.reverse $ hash

getTxHash :: Get TxHash
getTxHash = TxHash . BS.reverse <$> getByteString 32

defaultSequence :: Sequence
defaultSequence = Sequence 0xffffffff

instance Binary Sequence where
  put = putSequence
  get = getSequence

putSequence :: Sequence -> Put
putSequence (Sequence sequence') =
  putWord32le sequence'

getSequence :: Get Sequence
getSequence = Sequence <$> getWord32le

defaultLockTime = LockTime 0x00000000

instance Binary LockTime where
  put = putBlockLockTime
  get = getBlockLockTime

putBlockLockTime :: LockTime -> Put
putBlockLockTime (LockTime locktime') =
  putWord32le locktime'

getBlockLockTime :: Get LockTime
getBlockLockTime = LockTime <$> getWord32le

putSighashAll :: Put
putSighashAll =
  putWord8 1
