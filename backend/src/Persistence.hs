{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Persistence where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
                            share, sqlSettings)
import Database.Persist.Sql ( rawQuery
                            , insert
                            , SqlPersistT
                            , runSqlPool
                            , runSqlPersistMPool)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Server.Config (ConfigM, Config(..))
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)

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
    publicKey Text
    deriving Show
PersistentBlockHeader
    blockVersion Int
    prevBlockHash ByteString
    merkleRoot ByteString
    timestamp Int
    difficulty ByteString
    nonce ByteString
    txCount Int
|]
  

migrateSchema :: Config -> IO ()
migrateSchema c =
  liftIO $ flip runSqlPersistMPool (pool c) $ runMigration migrateTables


runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
         SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (runSqlPool q p)
