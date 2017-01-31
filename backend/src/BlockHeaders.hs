{-# LANGUAGE OverloadedStrings #-}
module BlockHeaders where

import Data.ByteString (ByteString(..))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Binary.Put (Put, putWord32le, putWord32be, putWord64le, putByteString, putWord8)
import Data.Binary.Get (Get(..), getWord32le, getByteString, getWord8)
import Data.Binary (Binary(..))
import Data.ByteString.Base16 (decode, encode)
------
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, suchThat, vectorOf, elements, oneof, listOf, Gen)
import qualified Data.ByteString as BS

data BlockHeader =
  BlockHeader BlockVersion PrevBlockHash MerkleRoot Timestamp Difficulty Nonce TxCount
  deriving (Eq, Show)

data BlockVersion = BlockVersion Int
  deriving (Eq, Show)

type PrevBlockHash = BlockHash

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

data TxCount = TxCount Int -- Always 0 for block headers
  deriving (Eq, Show)

putBlockHeader :: BlockHeader -> Put
putBlockHeader (BlockHeader version prevHash merkleRoot time difficulty nonce txCount) = do
  put version
  put prevHash
  put merkleRoot
  put time
  put difficulty
  put nonce
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
  putWord8 . fromIntegral $ count

getTxCount :: Get TxCount
getTxCount =
  TxCount . fromIntegral <$> getWord8

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
  arbitrary = TxCount <$> choose (0, maxCount)
    where maxCount = 0xff -- 1 byte
