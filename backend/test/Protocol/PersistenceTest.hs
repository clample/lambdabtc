{-# LANGUAGE OverloadedStrings #-}
module Protocol.PersistenceTest where

import TestUtil
import General.Persistence (migrateTables)
import BitcoinCore.BlockHeaders
import BitcoinCore.Transaction.Transactions
import Protocol.Persistence
import Protocol.Util (BlockIndex(..))

import Database.Persist.Sqlite ( createSqlitePool
                               , runMigrationSilent
                               , withSqliteConn)
import Database.Persist.Sql ( ConnectionPool
                            , runSqlPool
                            , SqlBackend)
import Control.Monad.Logger ( runStdoutLoggingT
                            , filterLogger
                            , LogLevel(..)
                            , LoggingT)
import Test.QuickCheck (ioProperty, Property)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT, ReaderT)

createTestDbPool :: IO ConnectionPool
createTestDbPool = do
  let newPool = createSqlitePool ":memory:" 1
      logFilter _ level = level == LevelError 
  pool <- runStdoutLoggingT . filterLogger logFilter $ newPool
  runSqlPool (runMigrationSilent migrateTables) pool
  return pool

runInMemoryProperty :: ReaderT SqlBackend IO Bool -> Property
runInMemoryProperty f = ioProperty
  . runStdoutLoggingT
  . filterLogger logFilter
  . withSqliteConn ":memory:" -- :: (SqlBackend -> LoggingT IO a) -> IO a
  . (fmap lift) -- change out put from IO Bool to LoggingT IO Bool 
  . runReaderT
  $ runMigrationSilent migrateTables >> f -- setup tables before running f
  where
    logFilter _ level = level == LevelError

persistAndRetrieveBlockHeader = buildTest . return $ testProperty
  "It should be possible to persist and retrieve a block header"
  (runInMemoryProperty . prop_persistAndRetrieveBlockHeader)

prop_persistAndRetrieveBlockHeader :: BlockHeader -> ReaderT SqlBackend IO Bool
prop_persistAndRetrieveBlockHeader header = do
  let hash = hashBlock header
  persistHeaderNonPool header
  mHeader' <- getBlockHeaderFromHashNonPool hash
  case mHeader' of
    Nothing -> return False
    Just (_, header') ->
      return (hashBlock header' == hash)

persistAndRetrieveTransaction = buildTest . return $ testProperty
    "It should be possible to persist and retrieve a transaction"
    (runInMemoryProperty . prop_persistAndRetrieveTransaction)

prop_persistAndRetrieveTransaction :: Transaction -> ReaderT SqlBackend IO Bool
prop_persistAndRetrieveTransaction tx = do
  let hash' = hashTransaction tx
  persistTransactionNonPool tx
  mTx' <- getTransactionFromHashNonPool hash'
  case mTx' of
    Nothing -> return False
    Just _ -> return True

persistAndGetLastBlock = buildTest . return $ testProperty
    "It should be possible to persist blocks and get the correct index from `getLastBlock`"
    (runInMemoryProperty . prop_persistAndGetLastBlock)

prop_persistAndGetLastBlock :: [BlockHeader] -> ReaderT SqlBackend IO Bool
prop_persistAndGetLastBlock headers = do
  (BlockIndex lastBlockInitial) <- getLastBlockNonPool
  persistHeadersNonPool headers
  (BlockIndex lastBlockFinal) <- getLastBlockNonPool
  return $ lastBlockFinal - lastBlockInitial == length headers

getBlockWithIndexAndHash = buildTest . return $ testProperty
    "We should obtain the same block whether querying by index or hash"
    (runInMemoryProperty . prop_getBlockWithIndexAndHash)

prop_getBlockWithIndexAndHash :: BlockHeader -> ReaderT SqlBackend IO Bool
prop_getBlockWithIndexAndHash header = do
  let hash = hashBlock header
  persistHeaderNonPool header
  mHeaderFromHash <- getBlockHeaderFromHashNonPool hash
  case mHeaderFromHash of
    Nothing -> return False
    Just (key, headerFromHash) -> do
      mHeaderFromIndex <- getBlockWithIndexNonPool key
      case mHeaderFromIndex of
        Nothing -> return False
        Just headerFromIndex -> return
          (headerFromIndex == headerFromHash)

deleteAndGetBlocksTest = buildTest $ do
  pool <- createTestDbPool
  return $ testProperty
    "Deleting blocks should not mess up indices when persisting new blocks"
    (prop_deleteAndGetBlocksTest pool)

prop_deleteAndGetBlocksTest :: ConnectionPool -> [BlockHeader] -> BlockHeader -> Property
prop_deleteAndGetBlocksTest pool initialHeaders newHeader = ioProperty $ do
  persistHeaders pool initialHeaders
  deleteHeaders pool (BlockIndex 0)
  persistHeader pool newHeader
  mFirstHeader <- getBlockWithIndex pool (BlockIndex 0)
  deleteHeaders pool (BlockIndex 0) -- clean db for next test
  case mFirstHeader of
    Nothing -> return False
    Just firstHeader -> return $
      hashBlock firstHeader == hashBlock newHeader
