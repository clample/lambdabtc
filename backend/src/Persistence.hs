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
import Database.Persist.Sql (rawQuery, insert)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
KeySet
    address Text
    privateKey Text
    publicKey Text
    deriving Show
|]

dumpTable = rawQuery "select * from key_set" [] $$ CL.mapM_ (liftIO . print)

runExample :: IO()
runExample = runSqlite ":memory:" $ do
  runMigration migrateTables
  insert $ KeySet "address123" "privateKey123" "publicKey123"
  dumpTable
