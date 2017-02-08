{-# LANGUAGE OverloadedStrings #-}
module LamdaBTC.Server (runApplication, developmentConfig) where

import LamdaBTC.Handlers 
import LamdaBTC.Config

import Web.Scotty.Trans ( ScottyT
                        , scottyOptsT
                        , middleware
                        , defaultHandler
                        , post
                        , get)
import Control.Monad.Reader (runReaderT)

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
  get  "/fundrequests" getFundRequestsH
