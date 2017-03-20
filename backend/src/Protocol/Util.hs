{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Protocol.Util where

import General.Persistence ( PersistentBlockHeader(..)
                           , PersistentUTXO(..))
import General.Hash (Hash(..), doubleSHA)
import BitcoinCore.BlockHeaders  ( BlockHeader(..)
                                 , BlockVersion(..)
                                 , Difficulty(..)
                                 , Nonce(..)
                                 , Timestamp(..)
                                 , hashBlock
                                 )
import BitcoinCore.MerkleTrees (MerkleHash(..))
import BitcoinCore.Transaction.Transactions ( Transaction(..)
                                            , outputScripts
                                            , hashTransaction)
import BitcoinCore.Transaction.Script (Script(..), ScriptComponent(..), putScript)

import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.List (lookup)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Binary (Binary(..))
import Data.Binary.Put (runPut, Put, putWord32le)
import Data.Binary.Get (Get, getWord32le)
import qualified Database.Persist.Sql as DB
import Control.Lens (Lens')

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements, choose)

decodeBlockHeader :: PersistentBlockHeader -> BlockHeader
decodeBlockHeader
  (PersistentBlockHeader
    blockVersion
    prevBlockHash
    hash
    merkleRoot
    timestamp
    difficulty
    nonce) =
  BlockHeader
    (BlockVersion blockVersion)
    (Hash prevBlockHash)
    (MerkleHash merkleRoot)
    (Timestamp . fromIntegral $ timestamp)
    (Difficulty difficulty)
    (Nonce nonce)

encodeBlockHeader :: BlockHeader -> PersistentBlockHeader
encodeBlockHeader
  header@(BlockHeader
    (BlockVersion blockVersion)
    (Hash prevBlockHash)
    (MerkleHash merkleRoot)
    (Timestamp timestamp)
    (Difficulty difficulty)
    (Nonce nonce)) =
  PersistentBlockHeader
    blockVersion
    prevBlockHash
    (hash . hashBlock $ header)
    merkleRoot
    (fromIntegral . floor $ timestamp)
    difficulty
    nonce

data CCode
  = REJECT_MALFORMED
  | REJECT_INVALID
  | REJECT_OBSOLETE
  | REJECT_DUPLICATE
  | REJECT_NONSTANDARD
  | REJECT_DUST
  | REJECT_INSUFFICIENTFEE
  | REJECT_CHECKPOINT
  deriving (Show, Eq)

instance Enum CCode where
  fromEnum = fromJust . flip lookup ccodeTable
  toEnum = fromJust . flip lookup (map swap ccodeTable)

ccodeTable :: [(CCode, Int)]
ccodeTable =
  [ (REJECT_MALFORMED, 0x01)
  , (REJECT_INVALID,   0x10)
  , (REJECT_OBSOLETE,  0x11)
  , (REJECT_DUPLICATE, 0x12)
  , (REJECT_NONSTANDARD, 0x40)
  , (REJECT_DUST, 0x41)
  , (REJECT_INSUFFICIENTFEE, 0x42)
  , (REJECT_CHECKPOINT, 0x43)]

getUTXOS :: [(Int, ByteString)] -> Transaction -> [PersistentUTXO]
getUTXOS indexedPubkeys transaction =
  map
    (\(pubkeyHashI, scriptI)
     -> getPersistentUTXO transaction scriptI (outputScripts transaction !! scriptI) pubkeyHashI)
    utxoIndices
  where
    indexedOutputScripts = zip [0..] (outputScripts transaction)
    utxoIndices =
      [ (pubkeyHashI, scriptI)
      | (pubkeyHashI, pubkeyHash) <- indexedPubkeys
      , (scriptI, script) <- indexedOutputScripts
      , isRelevantScript pubkeyHash script ]
        
isRelevantScript :: ByteString -> Script -> Bool
isRelevantScript pubkeyHash (Script components) =
  any isRelevantComponent components
  where isRelevantComponent (Txt bs) = bs == pubkeyHash
        isRelevantComponent (OP _) = False

getPersistentUTXO :: Transaction -> Int -> Script -> Int -> PersistentUTXO
getPersistentUTXO tx outIndex script = PersistentUTXO
  hash' outIndex scriptBS
  where
    hash' = hash . hashTransaction $ tx
    scriptBS = toStrict . runPut $ putScript script

instance Arbitrary CCode where
  arbitrary = elements . map fst $ ccodeTable

-- 0 based counting
-- genesis block has index 0
newtype BlockIndex = BlockIndex Int
  deriving (Show, Eq, Ord, Num, Enum)

-- instance Ord BlockIndex where
--   (BlockIndex a) <= (BlockIndex b) = a <= b



toDbKey :: BlockIndex -> DB.Key PersistentBlockHeader
toDbKey (BlockIndex i) = DB.toSqlKey . fromIntegral $ i + 1
  -- we add 1 since the db keys use 1 based counting

fromDbKey :: DB.Key PersistentBlockHeader -> BlockIndex
fromDbKey key = BlockIndex $ (fromIntegral . DB.fromSqlKey $ key) - 1
  -- we subtract 1 since the db keys use 1 base counting

class HasLastBlock t where
  lastBlock :: Lens' t BlockIndex

instance Binary BlockIndex where
  put = putBlockIndex
  get = getBlockIndex

getBlockIndex :: Get BlockIndex
getBlockIndex =
  BlockIndex . fromIntegral <$> getWord32le

putBlockIndex :: BlockIndex -> Put
putBlockIndex (BlockIndex i) =
  putWord32le . fromIntegral $ i

instance Arbitrary BlockIndex where
  arbitrary = BlockIndex <$> choose (0, maxBlock)
    where maxBlock = 0xffffffff -- 4 bytes
