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
import Test.QuickCheck (ioProperty, Property, Testable)
import Data.Text (append, unpack, Text)
import System.Directory (removeFile)

testDbs :: [Text]
testDbs = [ "resources/persistandretrieveblockheader.db"
          , "resources/persistandretrievetransaction.db"
          , "resources/persistandgetlastblock.db"
          , "resources/getblockwithindexandhash.db"
          , "resources/deleteandgetblocks.db"
          ]

createTestDbPool :: Text -> IO ConnectionPool
createTestDbPool testDBFile = do
  let newPool = createSqlitePool ("file:" `append` testDBFile) 1
      logFilter _ level = level == LevelError 
  pool <- runStdoutLoggingT . filterLogger logFilter $ newPool
  runSqlPool (runMigrationSilent migrateTables) pool
  return pool

cleanupTestDb :: Text -> IO ()
cleanupTestDb = removeFile . unpack 

buildDBTest :: Testable a => Text -> String -> (ConnectionPool -> a) -> Test
buildDBTest dbFile testStr testcase = buildTest $ do
  pool <- createTestDbPool dbFile
  return $ testProperty testStr $ testcase pool

persistAndRetrieveBlockHeader = buildDBTest
  (testDbs !! 0)
  "It should be possible to persist and retrieve a block header"
  prop_persistAndRetrieveBlockHeader

prop_persistAndRetrieveBlockHeader :: ConnectionPool -> BlockHeader -> Property
prop_persistAndRetrieveBlockHeader pool header = ioProperty $ do
  let hash = hashBlock header
  persistHeader pool header
  mHeader' <- getBlockHeaderFromHash pool hash
  case mHeader' of
    Nothing -> return False
    Just (_, header') ->
      return (hashBlock header' == hash)

persistAndRetrieveTransaction = buildDBTest
  (testDbs !! 1)
  "It should be possible to persist and retrieve a transaction"
  prop_persistAndRetrieveTransaction

prop_persistAndRetrieveTransaction :: ConnectionPool -> Transaction -> Property
prop_persistAndRetrieveTransaction pool tx = ioProperty $ do
  let hash' = hashTransaction tx
  persistTransaction pool tx
  mTx' <- getTransactionFromHash pool hash'
  case mTx' of
    Nothing -> return False
    Just _ -> return True

persistAndGetLastBlock = buildDBTest
  (testDbs !! 2)
  "It should be possible to persist blocks and get the correct index \
     \from `getLastBlock`"
  prop_persistAndGetLastBlock
  
prop_persistAndGetLastBlock :: ConnectionPool -> [BlockHeader] -> Property
prop_persistAndGetLastBlock pool headers = ioProperty $ do
  (BlockIndex lastBlockInitial) <- getLastBlock pool
  persistHeaders pool headers
  (BlockIndex lastBlockFinal) <- getLastBlock pool
  return $ lastBlockFinal - lastBlockInitial == length headers

getBlockWithIndexAndHash = buildDBTest
  (testDbs !! 3)
  "We should obtain the same block whether querying by index or hash"
  prop_getBlockWithIndexAndHash

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

deleteAndGetBlocksTest = buildDBTest
  (testDbs !! 4)
  "Deleting blocks should not mess up indices when persisting new blocks"
  prop_deleteAndGetBlocksTest

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
