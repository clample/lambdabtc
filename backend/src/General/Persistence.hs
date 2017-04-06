{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module General.Persistence where

import General.Config (ConfigM)
import General.Types (HasPool(..))

import Data.Text (Text)
import Data.Word (Word32)
import Database.Persist.Sqlite
  ( runMigration
  , runMigrationUnsafe
  )
import Database.Persist.TH
  ( mkPersist
  , mkMigrate
  , persistLowerCase
  , share
  , sqlSettings
  )
import Database.Persist.Sql
  ( SqlPersistT
  , ConnectionPool
  , runSqlPool
  , runSqlPersistMPool
  )
import Database.Persist (Entity(..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)
import Control.Lens ((^.))
import Data.Aeson.TH
  ( deriveJSON
  , defaultOptions
  )


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
    blockVersion Word32
    prevBlockHash ByteString
    hash ByteString
    merkleRoot ByteString
    timestamp Word32
    difficulty ByteString
    nonce ByteString
PersistentUTXO
    outTxHash ByteString
    outIndex Int
    script ByteString
    keySetId Int
    value Int
    isSpent Bool
PersistentTransaction
    hash ByteString
|]

data DisplayPersistentUTXO = DisplayPersistentUTXO { dispKeySetId :: Int
                                                   , dispValue :: Int
                                                   , dispIsSpent :: Bool
                                                   } deriving (Eq, Show)

deriveJSON defaultOptions ''DisplayPersistentUTXO

displayUTXO :: Entity PersistentUTXO -> DisplayPersistentUTXO
displayUTXO (Entity key utxo) = DisplayPersistentUTXO (persistentUTXOKeySetId utxo)
                                                      (persistentUTXOValue utxo)
                                                      (persistentUTXOIsSpent utxo)


migrateSchema :: ConnectionPool -> IO ()
migrateSchema =
  runSqlPersistMPool (runMigrationUnsafe migrateTables)


runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
         SqlPersistT IO a -> t ConfigM a
runDB q = do
  config <- lift ask
  liftIO (runSqlPool q (config^.pool))
