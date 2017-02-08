{-# LANGUAGE OverloadedStrings #-}
module BitcoinCore.BlockHeaders where

import General.Persistence
import General.Util (VarInt(..), doubleSHA)

import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Binary.Put (Put, putWord32le, putByteString, runPut)
import Data.Binary.Get (Get, getWord32le, getByteString)
import Data.Binary (Binary(..))
import Data.ByteString.Base16 (decode, encode)
------
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, vectorOf, Gen)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as Char8

data BlockHeader =
  BlockHeader BlockVersion PrevBlockHash MerkleRoot Timestamp Difficulty Nonce TxCount
  deriving (Eq, Show)

-- TODO: Just make BlockHeader a record? use lenses?
prevBlockHash :: BlockHeader -> BlockHash
prevBlockHash (BlockHeader _ hash _ _ _ _ _) = hash

data BlockVersion = BlockVersion Int
  deriving (Eq, Show)

type PrevBlockHash = BlockHash

-- BlockHash is binary encoded but we show it as hex encoded
data BlockHash = BlockHash ByteString
  deriving (Eq)

instance Show BlockHash where
  show (BlockHash bs) = "BlockHash " ++ (show . encode $ bs)

data MerkleRoot = MerkleRoot ByteString
  deriving (Eq)

instance Show MerkleRoot where
  show (MerkleRoot bs) = "MerkleRoot " ++ (show . encode $ bs)

data Timestamp = Timestamp POSIXTime
  deriving (Eq, Show)

data Difficulty = Difficulty ByteString
  deriving (Eq)

instance Show Difficulty where
  show (Difficulty bs) = "Difficulty " ++ (show . encode $ bs)

data Nonce = Nonce ByteString
  deriving (Eq)

instance Show Nonce where
  show (Nonce bs) = "Nonce " ++ (show . encode $ bs)

data TxCount = TxCount VarInt -- Always 0 for block headers
  deriving (Eq, Show)

hashBlock :: BlockHeader -> BlockHash
hashBlock blockHeader = BlockHash $
  BS.reverse . doubleSHA . BL.toStrict . runPut $ putBlockHeaderWithoutTxCount blockHeader
  -- HELP: Why do we need reverse here?

decodeBlockHeader :: PersistentBlockHeader -> BlockHeader
decodeBlockHeader
  (PersistentBlockHeader
    blockVersion
    prevBlockHash
    merkleRoot
    timestamp
    difficulty
    nonce
    txCount) =
  BlockHeader
    (BlockVersion blockVersion)
    (BlockHash prevBlockHash)
    (MerkleRoot merkleRoot)
    (Timestamp . fromIntegral $ timestamp)
    (Difficulty difficulty)
    (Nonce nonce)
    (TxCount . VarInt $ txCount)

encodeBlockHeader :: BlockHeader -> PersistentBlockHeader
encodeBlockHeader
  (BlockHeader
    (BlockVersion blockVersion)
    (BlockHash prevBlockHash)
    (MerkleRoot merkleRoot)
    (Timestamp timestamp)
    (Difficulty difficulty)
    (Nonce nonce)
    (TxCount (VarInt txCount))) =
  PersistentBlockHeader
    blockVersion
    prevBlockHash
    merkleRoot
    (fromIntegral . floor $ timestamp)
    difficulty
    nonce
    txCount

putBlockHeaderWithoutTxCount :: BlockHeader -> Put
putBlockHeaderWithoutTxCount (BlockHeader version prevHash merkleRoot time difficulty nonce _) = do
  put version
  put prevHash
  put merkleRoot
  put time
  put difficulty
  put nonce


putBlockHeader :: BlockHeader -> Put
putBlockHeader blockHeader@(BlockHeader version prevHash merkleRoot time difficulty nonce txCount) = do
  putBlockHeaderWithoutTxCount blockHeader
  put txCount

getBlockHeader :: Get BlockHeader
getBlockHeader = do
  version <- get
  prevHash <- get
  merkleRoot <- get
  time <- get
  difficulty <- get
  nonce <- get
  txCount <- get
  return $ BlockHeader version prevHash merkleRoot time difficulty nonce txCount

instance Binary BlockHeader where
  put = putBlockHeader
  get = getBlockHeader

putBlockVersion :: BlockVersion -> Put
putBlockVersion (BlockVersion v) =
  putWord32le (fromIntegral v)

getBlockVersion :: Get BlockVersion
getBlockVersion =
  BlockVersion . fromIntegral <$> getWord32le

instance Binary BlockVersion where
  put = putBlockVersion
  get = getBlockVersion

putBlockHash :: BlockHash -> Put
putBlockHash (BlockHash bs) =
  putByteString . BS.reverse $ bs

getBlockHash :: Get BlockHash
getBlockHash = BlockHash . BS.reverse <$> getByteString 32

instance Binary BlockHash where
  put = putBlockHash
  get = getBlockHash

putMerkleRoot :: MerkleRoot -> Put
putMerkleRoot (MerkleRoot bs) =
  putByteString bs

getMerkleRoot :: Get MerkleRoot
getMerkleRoot =
  MerkleRoot <$> getByteString 32

instance Binary MerkleRoot where
  put = putMerkleRoot
  get = getMerkleRoot

putTimestamp :: Timestamp -> Put
putTimestamp (Timestamp posixTime) =
  putWord32le . floor $ posixTime

getTimestamp :: Get Timestamp
getTimestamp =
  Timestamp . fromIntegral <$> getWord32le

instance Binary Timestamp where
  put = putTimestamp
  get = getTimestamp

putDifficulty :: Difficulty -> Put
putDifficulty (Difficulty bs) =
  putByteString bs

getDifficulty :: Get Difficulty
getDifficulty =
  Difficulty <$> getByteString 4

instance Binary Difficulty where
  put = putDifficulty
  get = getDifficulty

putNonce :: Nonce -> Put
putNonce (Nonce bs) =
  putByteString bs

getNonce :: Get Nonce
getNonce =
  Nonce <$> getByteString 4

instance Binary Nonce where
  put = putNonce
  get = getNonce

putTxCount :: TxCount -> Put
putTxCount (TxCount count) =
  put count

getTxCount :: Get TxCount
getTxCount =
  TxCount <$> get

-- We will assume that incoming headers are sorted [oldest ... newest]
-- TODO: try take any order headers and sort them if needed
verifyHeaders :: [BlockHeader] -> Bool
verifyHeaders [newest] = True
verifyHeaders (old:new:rest) =
  (hashBlock old == prevBlockHash new) &&
  verifyHeaders (new:rest)

instance Binary TxCount where
  put = putTxCount
  get = getTxCount

instance Arbitrary BlockHeader where
  arbitrary = BlockHeader
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary

instance Arbitrary BlockVersion where
  arbitrary = BlockVersion <$> choose (0, maxVersion)
    where maxVersion = 0xffffffff -- 4 bytes

instance Arbitrary BlockHash where
  arbitrary = BlockHash . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary MerkleRoot where
  arbitrary = MerkleRoot . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary Timestamp where
  arbitrary = Timestamp . realToFrac <$> (choose (0, maxTime) :: Gen Integer)
    where maxTime = 0xffffffff -- 4 bytes

instance Arbitrary Difficulty where
  arbitrary = Difficulty . BS.pack <$> vectorOf 4 arbitrary

instance Arbitrary Nonce where
  arbitrary = Nonce . BS.pack <$> vectorOf 4 arbitrary

instance Arbitrary TxCount where
  arbitrary = TxCount . VarInt <$> choose (0, maxCount)
    where maxCount = 0xff -- 1 byte

-- The genesis blocks were determined by hand, referencing
-- https://github.com/bitcoin/bitcoin/blob/812714fd80e96e28cd288c553c83838cecbfc2d9/src/chainparams.cpp
genesisBlockMain :: BlockHeader
genesisBlockMain = BlockHeader
  (BlockVersion 1)
  (BlockHash . fst . decode $ "0000000000000000000000000000000000000000000000000000000000000000" )
  (MerkleRoot . fst . decode $ "3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A")
  (Timestamp . fromIntegral $ 1231006505)
  (Difficulty . fst . decode $ "FFFF001D")
  (Nonce . fst . decode . Char8.pack $ "1DAC2B7C")
  (TxCount . VarInt $ 1)

genesisBlockTestnet :: BlockHeader
genesisBlockTestnet = BlockHeader
  (BlockVersion 1)
  (BlockHash . fst . decode $ "0000000000000000000000000000000000000000000000000000000000000000" )
  (MerkleRoot . fst . decode $ "3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A")
  (Timestamp . fromIntegral $ 1296688602)
  (Difficulty . fst . decode $ "FFFF001D")
  (Nonce . fst . decode $ "1aa4ae18")
  (TxCount . VarInt $ 1)
