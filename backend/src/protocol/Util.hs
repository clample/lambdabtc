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
                                 , TxCount(..))


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
