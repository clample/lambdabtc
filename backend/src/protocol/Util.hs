module Protocol.Util where

import General.Persistence ( PersistentBlockHeader(..)
                           , PersistentUTXO(..))
import BitcoinCore.BlockHeaders  ( BlockHeader(..)
                                 , BlockVersion(..)
                                 , BlockHash(..)
                                 , Difficulty(..)
                                 , Nonce(..)
                                 , Timestamp(..)
                                 , hashBlock)
import BitcoinCore.MerkleTrees (MerkleHash(..))
import BitcoinCore.Transaction.Transactions ( Transaction(..)
                                            , TxHash(..)
                                            , outputs
                                            , outputScript
                                            , hashTransaction
                                            , outputScripts)
import BitcoinCore.Transaction.Script (Script(..), ScriptComponent(..), putScript)

import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.List (lookup, findIndex)
import Control.Lens (over, mapped, (^.))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Binary (Binary(..))
import Data.Binary.Put (runPut)

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
    (BlockHash prevBlockHash)
    (MerkleHash merkleRoot)
    (Timestamp . fromIntegral $ timestamp)
    (Difficulty difficulty)
    (Nonce nonce)

encodeBlockHeader :: BlockHeader -> PersistentBlockHeader
encodeBlockHeader
  header@(BlockHeader
    (BlockVersion blockVersion)
    (BlockHash prevBlockHash)
    (MerkleHash merkleRoot)
    (Timestamp timestamp)
    (Difficulty difficulty)
    (Nonce nonce)) =
  PersistentBlockHeader
    blockVersion
    prevBlockHash
    hash
    merkleRoot
    (fromIntegral . floor $ timestamp)
    difficulty
    nonce
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
  or $ map isRelevantComponent components
  where isRelevantComponent (Txt bs) = bs == pubkeyHash
        isRelevantComponent (OP _) = False

getPersistentUTXO :: Transaction -> Int -> Script -> Int -> PersistentUTXO
getPersistentUTXO tx outIndex script keySetId' = PersistentUTXO
  hash outIndex scriptBS keySetId'
  where
    TxHash hash = hashTransaction tx
    scriptBS = toStrict . runPut $ putScript script
