module Protocol.Util where

import General.Persistence (PersistentBlockHeader(..))
import General.Util (VarInt(..))
import BitcoinCore.BlockHeaders  ( BlockHeader(..)
                                 , BlockVersion(..)
                                 , BlockHash(..)
                                 , MerkleRoot(..)
                                 , Difficulty(..)
                                 , Nonce(..)
                                 , Timestamp(..)
                                 , TxCount(..)
                                 , hashBlock)


import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.List (lookup)

decodeBlockHeader :: PersistentBlockHeader -> BlockHeader
decodeBlockHeader
  (PersistentBlockHeader
    blockVersion
    prevBlockHash
    hash
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
  header@(BlockHeader
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
    hash
    merkleRoot
    (fromIntegral . floor $ timestamp)
    difficulty
    nonce
    txCount
  where BlockHash hash = hashBlock header

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
