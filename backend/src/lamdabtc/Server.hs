{-# LANGUAGE OverloadedStrings #-}
module LamdaBTC.Server (runApplication) where

import LamdaBTC.Handlers 
import General.Config

import Web.Scotty.Trans ( ScottyT
                        , scottyOptsT
                        , middleware
                        , defaultHandler
                        , post
                        , get)
import Control.Monad.Reader (runReaderT)
import Control.Lens ((^.))

runApplication :: Config -> IO ()
runApplication c = do
  let r m = runReaderT (runConfigM m ) c
      app = application c
      o = getOptions c
  scottyOptsT o r app

application :: Config -> ScottyT Error ConfigM ()
application config = do
  middleware (loggingM (config^.environment))
  defaultHandler (defaultH (config^.environment))
  post "/fundrequests" postFundRequestsH
  get  "/fundrequests" getFundRequestsH
  post "/transactions" postTransactionsH
