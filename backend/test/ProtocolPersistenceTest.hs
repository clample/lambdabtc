{-# LANGUAGE OverloadedStrings #-}
module ProtocolPersistenceTest where

import TestUtil
import General.Persistence (migrateTables)
import BitcoinCore.BlockHeaders
import BitcoinCore.Transaction.Transactions
import Protocol.Persistence

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

persistAndRetrieveBlockHeader = testProperty
  "It should be possible to persist and retrieve a block header"
  prop_persistAndRetrieveBlockHeader

prop_persistAndRetrieveBlockHeader :: BlockHeader -> Property
prop_persistAndRetrieveBlockHeader header = ioProperty $ do
  pool <- createTestDbPool
  let hash = hashBlock header
  persistHeader pool header
  mHeader' <- getBlockHeaderFromHash pool hash
  case mHeader' of
    Nothing -> return False
    Just (_, header') ->
      return (hashBlock header' == hash)

persistAndRetrieveTransaction = testProperty
  "It should be possible to persist and retrieve a transaction"
  prop_persistAndRetrieveTransaction

prop_persistAndRetrieveTransaction :: Transaction -> Property
prop_persistAndRetrieveTransaction tx = ioProperty $ do
  pool <- createTestDbPool
  let hash = hashTransaction tx
  persistTransaction pool tx
  mTx' <- getTransactionFromHash pool hash
  case mTx' of
    Nothing -> return False
    Just _ -> return True
