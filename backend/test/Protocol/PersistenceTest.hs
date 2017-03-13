{-# LANGUAGE OverloadedStrings #-}
module Protocol.PersistenceTest where

import TestUtil
import General.Persistence (migrateTables)
import BitcoinCore.BlockHeaders
import BitcoinCore.Transaction.Transactions
import Protocol.Persistence
import Protocol.Util (BlockIndex(..))

import Database.Persist.Sqlite (createSqlitePool, runMigrationSilent)
import Database.Persist.Sql ( ConnectionPool
                            , runSqlPool)
import Control.Monad.Logger ( runStdoutLoggingT
                            , filterLogger
                            , LogLevel(..))
import Test.QuickCheck (ioProperty, Property)

createTestDbPool :: IO ConnectionPool
createTestDbPool = do
  let newPool = createSqlitePool ":memory:" 1
      logFilter _ level = level == LevelError 
  pool <- runStdoutLoggingT . filterLogger logFilter $ newPool
  runSqlPool (runMigrationSilent migrateTables) pool
  return pool

persistAndRetrieveBlockHeader = buildTest $ do
  pool <- createTestDbPool
  return $ testProperty
    "It should be possible to persist and retrieve a block header"
    (prop_persistAndRetrieveBlockHeader pool)

prop_persistAndRetrieveBlockHeader :: ConnectionPool -> BlockHeader -> Property
prop_persistAndRetrieveBlockHeader pool header = ioProperty $ do
  let hash = hashBlock header
  persistHeader pool header
  mHeader' <- getBlockHeaderFromHash pool hash
  case mHeader' of
    Nothing -> return False
    Just (_, header') ->
      return (hashBlock header' == hash)

persistAndRetrieveTransaction = buildTest $ do
  pool <- createTestDbPool
  return $ testProperty
    "It should be possible to persist and retrieve a transaction"
    (prop_persistAndRetrieveTransaction pool)

prop_persistAndRetrieveTransaction :: ConnectionPool -> Transaction -> Property
prop_persistAndRetrieveTransaction pool tx = ioProperty $ do
  let hash' = hashTransaction tx
  persistTransaction pool tx
  mTx' <- getTransactionFromHash pool hash'
  case mTx' of
    Nothing -> return False
    Just _ -> return True

persistAndGetLastBlock = buildTest $ do
  pool <- createTestDbPool
  return $ testProperty
    "It should be possible to persist blocks and get the correct index from `getLastBlock`"
    (prop_persistAndGetLastBlock pool)

prop_persistAndGetLastBlock :: ConnectionPool -> [BlockHeader] -> Property
prop_persistAndGetLastBlock pool headers = ioProperty $ do
  (BlockIndex lastBlockInitial) <- getLastBlock pool
  persistHeaders pool headers
  (BlockIndex lastBlockFinal) <- getLastBlock pool
  return $ lastBlockFinal - lastBlockInitial == length headers

getBlockWithIndexAndHash = buildTest $ do
  pool <- createTestDbPool
  return $ testProperty
    "We should obtain the same block whether querying by index or hash"
    (prop_getBlockWithIndexAndHash pool)

prop_getBlockWithIndexAndHash :: ConnectionPool -> BlockHeader -> Property
prop_getBlockWithIndexAndHash pool header = ioProperty $ do
  let hash = hashBlock header
  persistHeader pool header
  mHeaderFromHash <- getBlockHeaderFromHash pool hash
  case mHeaderFromHash of
    Nothing -> return False
    Just (key, headerFromHash) -> do
      mHeaderFromIndex <- getBlockWithIndex pool key
      case mHeaderFromIndex of
        Nothing -> return False
        Just headerFromIndex -> return
          (headerFromIndex == headerFromHash)
