{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Server (runApplication, developmentConfig) where

import Data.Text (unpack)
import Persistence
import Database.Persist.Sqlite (runSqlite, runMigration)
import Data.Text (Text)
import Web.Scotty.Trans ( ScottyT
                        , Options(..)
                        , scottyOptsT
                        , middleware
                        , defaultHandler
                        , post
                        , ActionT
                        , status
                        , showError
                        , json)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader)
import qualified Data.Text.Lazy as LT
import Server.Handlers (defaultH, postFundRequestsH)
import Server.Config

runApplication :: Config -> IO ()
runApplication c = do
  let r m = runReaderT (runConfigM m ) c
      app = application c
      o = getOptions c
  scottyOptsT o r app

application :: Config -> ScottyT Error ConfigM ()
application c = do
  let e = environment c
  middleware (loggingM e)
  defaultHandler (defaultH e)
  post "/fundrequests" postFundRequestsH
