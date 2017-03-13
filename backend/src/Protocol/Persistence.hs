module Protocol.Persistence where

import General.Config (Config(..))
import General.Persistence ( PersistentBlockHeader(..)
                           , KeySet(..)
                           , EntityField(..)
                           , PersistentUTXO
                           , PersistentTransaction(..))
import General.Types (HasNetwork(..), HasPool(..))
import General.Hash (Hash(..))
import BitcoinCore.Keys (Address(..))
import BitcoinCore.BlockHeaders (genesisBlock, BlockHeader(..), BlockHash(..))
import BitcoinCore.Transaction.Transactions ( Transaction(..)
                                            , TxHash
                                            , hashTransaction) 
import Protocol.Util ( encodeBlockHeader
                     , decodeBlockHeader
                     , BlockIndex(..)
                     , toDbKey
                     , fromDbKey)

import Database.Persist.Sql ( insertMany_
                            , insert_
                            , count
                            , runSqlPool
                            , Filter
                            , insert_
                            , selectList
                            , (==.)
                            , (>=.)
                            , ConnectionPool)
import qualified Database.Persist.Sql as DB
import Database.Persist.Types (SelectOpt(..))
import Control.Lens ((^.))
import Control.Monad (when)
import Data.List.Split (chunksOf)

-- Return the index for the most recent persisted block
getLastBlock :: ConnectionPool -> IO BlockIndex
getLastBlock pool = do
  blockCount <-  runSqlPool lastBlockQuery pool
  return . BlockIndex $ blockCount - 1
  where lastBlockQuery = count allBlocksFilter
        allBlocksFilter = [] :: [Filter PersistentBlockHeader]

persistGenesisBlock :: Config -> IO ()
persistGenesisBlock config = do
  lastBlock' <- getLastBlock (config^.pool)
  when (lastBlock' == BlockIndex 0) $
    runSqlPool (insert_ . encodeBlockHeader . genesisBlock $ (config^.network)) (config^.pool)

persistHeader :: ConnectionPool -> BlockHeader -> IO ()
persistHeader pool header = do
  runSqlPool (insert_ $ encodeBlockHeader header) pool

persistHeaders :: ConnectionPool -> [BlockHeader] -> IO ()
persistHeaders pool headers = do
  let persistentHeaders = map encodeBlockHeader headers
      chunkedPersistentHeaders = chunksOf 100 persistentHeaders
      -- Headers are inserted in chunks
      -- sqlite rejects if we insert all at once
  runSqlPool (mapM_ insertMany_ chunkedPersistentHeaders) pool

getBlockHeaderFromHash :: ConnectionPool -> BlockHash -> IO (Maybe (BlockIndex, BlockHeader))
getBlockHeaderFromHash pool (Hash hash') = do
  matches <- runSqlPool (selectList [PersistentBlockHeaderHash ==. hash'] []) pool
  case matches of
    []       -> return Nothing
    [header] -> do
      let DB.Entity persistentKey persistentHeader = header
          key = fromDbKey $ persistentKey
          blockHeader = decodeBlockHeader persistentHeader
      return . Just $ (key , blockHeader) 
    _        -> fail "Multiple blocks found with same hash."

getTransactionFromHash :: ConnectionPool -> TxHash -> IO (Maybe Integer)
getTransactionFromHash pool (Hash hash') = do
  matches <- runSqlPool (selectList [PersistentTransactionHash ==. hash'] []) pool
  case matches of
    [] -> return Nothing
    [tx] -> do
      let DB.Entity persistentKey _ = tx
          key = fromIntegral . DB.fromSqlKey $ persistentKey
      return . Just $ key
    _ -> fail "Multiple transactions found with same hash."

persistTransaction :: ConnectionPool -> Transaction -> IO ()
persistTransaction pool transaction =
  runSqlPool (insert_ persistentTransaction) pool
  where persistentTransaction = PersistentTransaction hash'
        hash' = hash . hashTransaction $ transaction

getBlockWithIndex :: ConnectionPool -> BlockIndex -> IO (Maybe BlockHeader)
getBlockWithIndex pool i = (fmap . fmap) decodeBlockHeader $
  runSqlPool (DB.get . toDbKey $ i) pool
  
nHeadersSinceKey :: ConnectionPool -> Int -> BlockIndex -> IO [BlockHeader]
nHeadersSinceKey pool n key = do
  let key' = toDbKey $ key
  persistentHeaders <- runSqlPool (selectList [ PersistentBlockHeaderId >=. key'] [LimitTo n]) pool
  return $ map getHeaderFromEntity persistentHeaders

getHeaderFromEntity :: DB.Entity PersistentBlockHeader -> BlockHeader
getHeaderFromEntity (DB.Entity _ persistentHeader) = decodeBlockHeader persistentHeader

getAllAddresses :: ConnectionPool -> IO [Address]
getAllAddresses pool = do
  let allAddressFilter = [] :: [Filter KeySet]
  keySetEntities <- runSqlPool (selectList allAddressFilter []) pool
  let getAddress (DB.Entity _ keySet) = Address . keySetAddress $ keySet
  return $ map getAddress keySetEntities

persistUTXOs :: ConnectionPool -> [PersistentUTXO] -> IO ()
persistUTXOs pool utxos = runSqlPool (insertMany_ utxos) pool
