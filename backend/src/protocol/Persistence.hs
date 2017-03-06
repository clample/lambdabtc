module Protocol.Persistence where

import General.Config (Config(..), pool)
import General.Persistence ( PersistentBlockHeader(..)
                           , KeySet(..)
                           , EntityField(..)
                           , PersistentUTXO
                           , PersistentTransaction(..))
import General.Types (HasNetwork(..))
import BitcoinCore.Keys (Address(..))
import BitcoinCore.BlockHeaders (genesisBlock, BlockHeader(..), BlockHash(..))
import BitcoinCore.Transaction.Transactions ( Transaction(..)
                                            , TxHash(..)
                                            , hashTransaction)
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
                            , (>=.)
                            , ConnectionPool)
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

getBlockHeaderFromHash :: ConnectionPool -> BlockHash -> IO (Maybe (Integer, BlockHeader))
getBlockHeaderFromHash pool (BlockHash hash) = do
  matches <- runSqlPool (selectList [PersistentBlockHeaderHash ==. hash] []) pool
  case matches of
    []       -> return Nothing
    [header] -> do
      let DB.Entity persistentKey persistentHeader = header
          key = fromIntegral . DB.fromSqlKey $ persistentKey
          blockHeader = decodeBlockHeader persistentHeader
      return . Just $ (key , blockHeader) 
    _        -> fail "Multiple blocks found with same hash."

getTransactionFromHash :: ConnectionPool -> TxHash -> IO (Maybe Integer)
getTransactionFromHash pool (TxHash hash) = do
  matches <- runSqlPool (selectList [PersistentTransactionHash ==. hash] []) pool
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
  where persistentTransaction = PersistentTransaction hash
        TxHash hash = hashTransaction transaction

-- TODO: This won't compose with `getBlockHeaderFromHash`!
--       We are using 0 based indexing and `getBlockHeaderFromHash` is
--       presumably just using the db indexing (which is presumably 1 based)
getBlockWithIndex :: ConnectionPool -> Int -> IO (Maybe PersistentBlockHeader)
getBlockWithIndex pool i =
  runSqlPool (DB.get (toSqlKey . fromIntegral $  i + 1)) pool
  -- NOTE: we query by i + 1 since the genesis block (block 0) is in the db at index 1
  
-- TODO: Get better type signature to tell n and key apart
nHeadersSinceKey :: ConnectionPool -> Int -> Integer -> IO [BlockHeader]
nHeadersSinceKey pool n key = do
  let key' = toSqlKey . fromIntegral $ key
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
