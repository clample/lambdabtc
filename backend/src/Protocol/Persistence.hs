module Protocol.Persistence where

import General.Config (Config(..))
import General.Persistence
  ( PersistentBlockHeader(..)
  , KeySet(..)
  , EntityField(..)
  , PersistentUTXO
  , PersistentTransaction(..)
  )
import General.Types (HasNetwork(..), HasPool(..))
import General.Hash (Hash(..))
import BitcoinCore.Keys (Address(..))
import BitcoinCore.BlockHeaders
  ( genesisBlock
  , BlockHeader(..)
  , BlockHash(..)
  )
import BitcoinCore.Transaction.Transactions
  ( Transaction(..)
  , TxHash
  , hashTransaction
  ) 
import Protocol.Util
  ( encodeBlockHeader
  , decodeBlockHeader
  , BlockIndex(..)
  , toDbKey
  , fromDbKey
  )

import Database.Persist.Sql
  ( insertMany_
  , insert_
  , count
  , runSqlPool
  , Filter
  , insert_
  , selectList
  , update
  , (==.)
  , (=.)
  , (>=.)
  , ConnectionPool
  , SqlBackend
  )

import qualified Database.Persist.Sql as DB
import Database.Persist.Types (SelectOpt(..))
import Control.Lens ((^.))
import Control.Monad (when)
import Data.List.Split (chunksOf)
import Control.Monad.Trans.Reader (ReaderT)

-- Return the index for the most recent persisted block
getLastBlockNonPool :: ReaderT SqlBackend IO BlockIndex
getLastBlockNonPool = do
  blockCount <- count ([] :: [Filter PersistentBlockHeader])
  return . BlockIndex $ blockCount - 1

getLastBlock :: ConnectionPool -> IO BlockIndex
getLastBlock = runSqlPool getLastBlockNonPool

persistGenesisBlock :: Config -> IO ()
persistGenesisBlock config = do
  lastBlock' <- getLastBlock (config^.pool)
  when (lastBlock' < BlockIndex 0) $
    runSqlPool (insert_ . encodeBlockHeader . genesisBlock $ (config^.network)) (config^.pool)

persistHeaderNonPool :: BlockHeader -> ReaderT SqlBackend IO ()
persistHeaderNonPool = insert_ . encodeBlockHeader

persistHeader :: ConnectionPool -> BlockHeader -> IO ()
persistHeader pool header = do
  runSqlPool (persistHeaderNonPool header) pool

persistHeadersNonPool :: [BlockHeader] -> ReaderT SqlBackend IO ()
persistHeadersNonPool headers = do
  let persistentHeaders = map encodeBlockHeader headers
      chunkedPersistentHeaders = chunksOf 100 persistentHeaders
  mapM_ insertMany_ chunkedPersistentHeaders

persistHeaders :: ConnectionPool -> [BlockHeader] -> IO ()
persistHeaders pool headers = runSqlPool (persistHeadersNonPool headers) pool

-- | deletes all headers with index >= inx.
deleteHeaders :: ConnectionPool -> BlockIndex -> IO ()
deleteHeaders pool inx = do
  lastBlock <- getLastBlock pool
  let inxs = enumFromTo inx lastBlock
  runSqlPool (mapM_ (DB.delete . toDbKey) inxs) pool

getBlockHeaderFromHashNonPool :: BlockHash -> ReaderT SqlBackend IO (Maybe (BlockIndex, BlockHeader))
getBlockHeaderFromHashNonPool (Hash hash') = do
  matches <- selectList [PersistentBlockHeaderHash ==. hash'] []
  case matches of
    []       -> return Nothing
    [header] -> do
      let DB.Entity persistentKey persistentHeader = header
          key = fromDbKey persistentKey
          blockHeader = decodeBlockHeader persistentHeader
      return . Just $ (key, blockHeader)
    _        -> fail "Multiple blocks found with same hash."

getBlockHeaderFromHash :: ConnectionPool -> BlockHash -> IO (Maybe (BlockIndex, BlockHeader))
getBlockHeaderFromHash pool hash = runSqlPool (getBlockHeaderFromHashNonPool hash) pool  

getTransactionFromHashNonPool :: TxHash -> ReaderT SqlBackend IO (Maybe Integer)
getTransactionFromHashNonPool (Hash hash') = do
  matches <- selectList [PersistentTransactionHash ==. hash'] []
  case matches of
    [] -> return Nothing
    [tx] -> do
      let DB.Entity persistentKey _ = tx
          key = fromIntegral . DB.fromSqlKey $ persistentKey
      return . Just $ key
    _ -> fail "Multiple transactions found with same hash."

getTransactionFromHash :: ConnectionPool -> TxHash -> IO (Maybe Integer)
getTransactionFromHash pool h =
  runSqlPool (getTransactionFromHashNonPool h) pool

persistTransactionNonPool :: Transaction -> ReaderT SqlBackend IO ()
persistTransactionNonPool transaction = insert_ persistentTransaction
  where
    persistentTransaction = PersistentTransaction hash'
    hash' = hash . hashTransaction $ transaction

persistTransaction :: ConnectionPool -> Transaction -> IO ()
persistTransaction pool transaction = 
  runSqlPool (persistTransactionNonPool transaction) pool 

getBlockWithIndexNonPool :: BlockIndex 
                         -> ReaderT SqlBackend IO (Maybe BlockHeader)
getBlockWithIndexNonPool i = (fmap . fmap) decodeBlockHeader
                           . DB.get . toDbKey $ i

getBlockWithIndex :: ConnectionPool -> BlockIndex -> IO (Maybe BlockHeader)
getBlockWithIndex pool i = runSqlPool (getBlockWithIndexNonPool i) pool
  
nHeadersSinceKey :: ConnectionPool -> Int -> BlockIndex -> IO [BlockHeader]
nHeadersSinceKey pool n key = do
  let key' = toDbKey key
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

getUnspentUTXOs :: ConnectionPool -> IO [DB.Entity PersistentUTXO]
getUnspentUTXOs pool = do
  let unspentUTXOFilter = [ PersistentUTXOIsSpent ==. False]
  runSqlPool (selectList unspentUTXOFilter []) pool

setUtxoSpent :: ConnectionPool -> DB.Key PersistentUTXO -> IO ()
setUtxoSpent pool key =
  runSqlPool (update key [PersistentUTXOIsSpent =. True]) pool
