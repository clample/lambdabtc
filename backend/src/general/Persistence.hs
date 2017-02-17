{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module General.Persistence where

import General.Config (ConfigM, Config(..), pool)

import Data.Text (Text)
import Database.Persist.Sqlite (runMigration, runMigrationUnsafe)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
                            share, sqlSettings)
import Database.Persist.Sql ( SqlPersistT
                            , runSqlPool
                            , runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)
import Control.Lens ((^.))


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
FundRequest json
    label Text
    message Text
    amount Double
    address Text
    requestURI Text
KeySet
    address Text
    privateKey Text
    deriving Show
PersistentBlockHeader
    blockVersion Int
    prevBlockHash ByteString
    hash ByteString
    merkleRoot ByteString
    timestamp Int
    difficulty ByteString
    nonce ByteString
PersistentUTXO
    outTxHash ByteString
    outIndex Int
    script ByteString
    keySetId Int
PersistentTransaction
    hash ByteString
|]


migrateSchema :: Config -> IO ()
migrateSchema config =
  liftIO $ flip runSqlPersistMPool (config^.pool) $ runMigrationUnsafe migrateTables


runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
         SqlPersistT IO a -> t ConfigM a
runDB q = do
  config <- lift ask
  liftIO (runSqlPool q (config^.pool))
