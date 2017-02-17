module Protocol.Persistence where

import General.Config (Config(..), pool)
import General.Persistence (runDB, PersistentBlockHeader(..), KeySet(..), EntityField(..))
import General.Types (HasNetwork(..))
import BitcoinCore.Keys (Address(..))
import BitcoinCore.BlockHeaders (genesisBlock, BlockHeader(..), BlockHash(..))
import Protocol.ConnectionM (Connection)
import Protocol.Util (encodeBlockHeader, decodeBlockHeader)

import Database.Persist.Sql ( insertMany_
                            , insert_
                            , count
                            , runSqlPool
                            , Filter
                            , toSqlKey
                            , insert_
                            , selectList
                            , (==.)
                            , (>=.))
import qualified Database.Persist.Sql as DB
import Database.Persist.Types (SelectOpt(..))
import Control.Lens ((^.))
import Control.Monad (when)
import Data.List.Split (chunksOf)

getLastBlock :: Config -> IO Int
getLastBlock config =
  ((-1) +) <$> runSqlPool lastBlockQuery (config^.pool)
  where lastBlockQuery = count allBlocksFilter
        allBlocksFilter = [] :: [Filter PersistentBlockHeader]

persistGenesisBlock :: Config -> IO ()
persistGenesisBlock config = do
  lastBlock' <- getLastBlock config
  when (lastBlock' == -1) $
    runSqlPool (insert_ . encodeBlockHeader . genesisBlock $ (config^.network)) (config^.pool)

persistHeader :: BlockHeader -> Connection ()
persistHeader header = do
  runDB $ insert_ $ encodeBlockHeader header

persistHeaders :: [BlockHeader] -> Connection ()
persistHeaders headers = do
  let persistentHeaders = map encodeBlockHeader headers
      chunkedPersistentHeaders = chunksOf 100 persistentHeaders
      -- Headers are inserted in chunks
      -- sqlite rejects if we insert all at once
  runDB $ mapM_ insertMany_ chunkedPersistentHeaders

-- returns the leftmost header that we are currently persisting
firstHeaderMatch :: [BlockHash] -> Connection (DB.Entity PersistentBlockHeader)
firstHeaderMatch [] = fail "No matching hash was found"
firstHeaderMatch (hash:hashes) = do
  mHeader <- haveHeader hash
  case mHeader of
    Just header -> return header
    Nothing -> firstHeaderMatch hashes

haveHeader :: BlockHash -> Connection (Maybe (DB.Entity PersistentBlockHeader))
haveHeader (BlockHash hash) = do
  matches <- runDB $ selectList [PersistentBlockHeaderHash ==. hash] []
  case matches of
    []       -> return Nothing
    [header] -> return $ Just header
    _        -> fail "Multiple blocks found with same hash"

getBlockWithIndex :: Int -> Connection (Maybe PersistentBlockHeader)
getBlockWithIndex i =
  runDB $ DB.get (toSqlKey . fromIntegral $  i + 1)
  -- NOTE: we query by i + 1 since the genesis block (block 0) is in the db at index 1

nHeadersSince :: Int -> DB.Entity PersistentBlockHeader -> Connection [DB.Entity PersistentBlockHeader]
nHeadersSince n (DB.Entity headerId _) =
  runDB $ selectList [ PersistentBlockHeaderId >=. headerId ] [LimitTo n]

getHeaderFromEntity :: DB.Entity PersistentBlockHeader -> BlockHeader
getHeaderFromEntity (DB.Entity _ persistentHeader) = decodeBlockHeader persistentHeader

getAllAddresses :: Connection [Address]
getAllAddresses = do
  let allAddressFilter = [] :: [Filter KeySet]
  keySetEntities <- runDB $ selectList allAddressFilter []
  let getAddress (DB.Entity _ keySet) = Address . keySetAddress $ keySet
  return $ map getAddress keySetEntities
