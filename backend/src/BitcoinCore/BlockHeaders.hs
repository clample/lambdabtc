{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module BitcoinCore.BlockHeaders where

import General.Types (Network(..))
import BitcoinCore.MerkleTrees (MerkleHash(..))
import General.Hash (Hash(..), hashObject, doubleSHA)
import General.Util (Tree(..))

import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Binary.Put (Put, putWord32le, putByteString)
import Data.Binary.Get (Get, getWord32le, getByteString)
import Data.Binary (Binary(..))
import Data.ByteString.Base16 (decode, encode)
import Control.Lens (makeLenses, (^.))
import Control.Monad (replicateM)
import Data.Word(Word32)
-----------
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, vectorOf, Gen, sized)
import qualified Data.ByteString as BS

data BlockHeader = BlockHeader
  { _blockVersion :: BlockVersion
  , _prevBlockHash :: PrevBlockHash
  , _merkleRoot :: MerkleHash
  , _timestamp :: Timestamp
  , _difficulty :: Difficulty
  , _nonce :: Nonce
  } deriving (Eq, Show)

newtype BlockVersion = BlockVersion Word32
  deriving (Eq, Show)

type PrevBlockHash = BlockHash
type BlockHash = Hash BlockHeader

newtype Timestamp = Timestamp POSIXTime
  deriving (Eq, Show)

newtype Difficulty = Difficulty ByteString
  deriving (Eq)

instance Show Difficulty where
  show (Difficulty bs) = "Difficulty " ++ (show . encode $ bs)

newtype Nonce = Nonce ByteString
  deriving (Eq)

instance Show Nonce where
  show (Nonce bs) = "Nonce " ++ (show . encode $ bs)

makeLenses ''BlockHeader

hashBlock :: BlockHeader -> Hash BlockHeader
hashBlock = hashObject doubleSHA

putBlockHeader :: BlockHeader -> Put
putBlockHeader blockHeader = do
  put $ blockHeader^.blockVersion
  put $ blockHeader^.prevBlockHash
  put $ blockHeader^.merkleRoot
  put $ blockHeader^.timestamp
  put $ blockHeader^.difficulty
  put $ blockHeader^.nonce

getBlockHeader :: Get BlockHeader
getBlockHeader = do
  blockVersion' <- get
  prevBlockHash' <- get
  merkleRoot' <- get
  timestamp' <- get
  difficulty' <- get
  nonce' <- get
  return $ BlockHeader blockVersion' prevBlockHash' merkleRoot' timestamp' difficulty' nonce'

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

-- We will assume that incoming headers are sorted [oldest ... newest]
-- TODO: try take any order headers and sort them if needed
verifyHeaders :: [BlockHeader] -> Bool
verifyHeaders [] = error $ "We should never need to verify an empty list of headers"
verifyHeaders [newest] = True
verifyHeaders (old:new:rest) =
  (hashBlock old == (new^.prevBlockHash)) &&
  verifyHeaders (new:rest)

instance Arbitrary BlockHeader where
  arbitrary = BlockHeader
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary

instance Arbitrary BlockVersion where
  arbitrary = BlockVersion <$> choose (0, maxVersion)
    where maxVersion = 0xffffffff -- 4 bytes

instance Arbitrary Timestamp where
  arbitrary = Timestamp . realToFrac <$> (choose (0, maxTime) :: Gen Integer)
    where maxTime = 0xffffffff -- 4 bytes

instance Arbitrary Difficulty where
  arbitrary = Difficulty . BS.pack <$> vectorOf 4 arbitrary

instance Arbitrary Nonce where
  arbitrary = Nonce . BS.pack <$> vectorOf 4 arbitrary

newtype ValidHeaders = ValidHeaders [BlockHeader]
  deriving (Show)

instance Arbitrary ValidHeaders where
  arbitrary = do
    oldestHeader <- arbitrary :: Gen BlockHeader
    let header1 = oldestHeader
    header2 <- nextHeader header1
    header3 <- nextHeader header2
    header4 <- nextHeader header3
    let validHeaders = [header1, header2, header3, header4]
    return $ ValidHeaders validHeaders

nextHeader :: BlockHeader -> Gen BlockHeader
nextHeader h =
  BlockHeader
  <$> arbitrary
  <*> pure (hashBlock h)
  <*> arbitrary
  <*> arbitrary
  <*> arbitrary
  <*> arbitrary

newtype ValidBlockTree = ValidBlockTree (Tree BlockHeader)
  deriving (Show, Eq)

instance Arbitrary ValidBlockTree where
  arbitrary = do
    genesisBlock <- arbitrary
    ValidBlockTree <$> (sized $ arbBlockTree genesisBlock)

arbBlockTree :: BlockHeader -> Int -> Gen (Tree BlockHeader)
arbBlockTree parentBlock 0 = do
  next <- nextHeader parentBlock
  return $ Tree
    { _node = next
    , _subTree = []
    }
arbBlockTree parentBlock n = do
  next     <- nextHeader parentBlock
  l1 <- choose (0, n `div` 2)
  l2 <- choose (0, n `div` 2)
  subTree' <- replicateM l1 $ arbBlockTree next l2
  return $ Tree
    { _node = next
    , _subTree = subTree'
    }

{--
-- all headers from ValidBlockTree will
-- be included in the result
splitTree :: Tree BlockHeader -> Gen [[BlockHeader]]
splitTree (ValidBlockTree tree) = return $ branches tree
--}

-- The genesis blocks were determined by hand, referencing
-- https://github.com/bitcoin/bitcoin/blob/812714fd80e96e28cd288c553c83838cecbfc2d9/src/chainparams.cpp
genesisBlock :: Network -> BlockHeader
genesisBlock MainNet = BlockHeader
  (BlockVersion 1)
  (Hash . fst . decode $ "0000000000000000000000000000000000000000000000000000000000000000" )
  (MerkleHash . fst . decode $ "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
  (Timestamp . fromIntegral $ 1231006505)
  (Difficulty . fst . decode $ "FFFF001D")
  (Nonce . fst . decode $ "1DAC2B7C")

genesisBlock TestNet3 = BlockHeader
  (BlockVersion 1)
  (Hash . fst . decode $ "0000000000000000000000000000000000000000000000000000000000000000" )
  (MerkleHash . fst . decode $ "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
  (Timestamp . fromIntegral $ 1296688602)
  (Difficulty . fst . decode $ "FFFF001D")
  (Nonce . fst . decode $ "1aa4ae18")

showBlocks = unwords . map (show . hashBlock)
