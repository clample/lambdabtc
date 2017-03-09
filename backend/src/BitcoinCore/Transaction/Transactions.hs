{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module BitcoinCore.Transaction.Transactions where

import General.Util
import BitcoinCore.Transaction.Script
import BitcoinCore.Keys (serializePublicKeyRep, PublicKeyRep(..), PubKeyFormat(..))
import General.Hash (Hash(..), hashObject)

import Prelude hiding (concat, reverse, sequence)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Crypto.PubKey.ECC.ECDSA (signWith, Signature(..), PrivateKey(..), PublicKey(..))
import Crypto.Hash.Algorithms (SHA256(..))

import Control.Lens (makeLenses, (^.), mapped, set)
import Data.Binary.Put (Put, putWord8, putWord32le, putWord64le, putByteString, runPut)
import Data.Binary.Get (Get, getWord32le, getByteString, getWord64le, getWord8)
import Data.Binary (Binary(..), Word32)
import Control.Monad (replicateM)
import Data.Bits ((.&.))
import Crypto.Hash (hashWith)
import Data.ByteArray (convert)

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, vectorOf, suchThat)

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

type TxHash = Hash Transaction

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

-------------------- Transaction signing
signedTransaction :: UTXO -> Script -> (PublicKey, PrivateKey) -> [TxOutput] -> Transaction
signedTransaction utxo' oldInputScript keys outputs'  = 
  (set (inputs.mapped.signatureScript) newInputScript transaction)
  where
    newInputScript = scriptSig (signedHash oldInputScript (snd keys) transaction) (fst keys)
    
    transaction = Transaction
      {_inputs = [TxInput { _utxo = utxo'
                          , _sequence = defaultSequence}]
      , _outputs = outputs'
      , _txVersion = TxVersion 1
      , _locktime = defaultLockTime}

-- We need to sign the double SHA the intermediateTransaction.
-- Since `signWith` always performs a SHA, we achieve correct
-- behaviour by performing one SHA in `intermediateHash`
-- and allowing `signWith` to perform the second hash
signedHash :: Script -> PrivateKey -> Transaction -> Signature
signedHash oldInputScript privateKey intermediateTransaction =
  case signWith 100 privateKey SHA256 intermediateHash' of
    Nothing -> error "Unable to sign hash"
    Just sig -> sig
  where intermediateHash' = intermediateHash intermediateTransaction oldInputScript

intermediateHash :: Transaction -> Script -> ByteString
intermediateHash intermediateTransaction oldInputScript =
  convert . (hashWith SHA256) $ bs
  where bs = BL.toStrict . runPut $ do
          put (set (inputs.mapped.signatureScript) oldInputScript intermediateTransaction)
          putWord32le sighashAll

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
  put (utxo'^.outTxHash)
  let TxIndex i = utxo'^.outIndex
  putWord32le . fromIntegral $ i

getOutPoint :: Get UTXO
getOutPoint = UTXO
  <$> get
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
  where der = (BL.toStrict . runPut $ putDerSignature signature)
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
  putWithLength (putDERContents signature)
  putWord8 0x01 -- one byte hashcode type

putDERInt :: Integer -> Put
putDERInt int = do
  let intBS = unroll BE int
      headByte = BS.head intBS
  if (headByte .&. 0x80 == 0x80)
    then putByteString $ 0x00 `BS.cons` intBS
    else putByteString intBS
    
putDERContents signature = do
  putWord8 0x02
  putWithLength
    (putDERInt . sign_r $ signature)
  putWord8 0x02
  putWithLength
    (putDERInt . getLowS . sign_s $ signature)
    
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
  getWord8
  yLength <- fromIntegral <$> getWord8
  y <- roll BE <$> getByteString yLength
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

defaultLockTime :: LockTime
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

instance Arbitrary UTXO where
  arbitrary = do
    hash <- arbitrary
    index <- TxIndex <$> choose (0, 0xffffffff)
    return UTXO
      { _outTxHash = hash
      , _outIndex = index }

instance Arbitrary Value where
  arbitrary = Satoshis <$> arbitrary `suchThat` (> 0)
