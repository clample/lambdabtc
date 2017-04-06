{-# LANGUAGE OverloadedStrings #-}
module LamdaBTC.Server (runApplication) where

import LamdaBTC.Handlers
import General.Config
import General.InternalMessaging (UIUpdaterMessage(..))

import Web.Scotty.Trans ( ScottyT
                        , scottyOptsT
                        , middleware
                        , defaultHandler
                        , post
                        , get)
import Control.Monad.Reader (runReaderT)
import Control.Lens ((^.))
import Network.WebSockets ( acceptRequest
                          , runServer
                          , Connection)
import Control.Concurrent.STM (atomically)
import Data.Conduit.TMChan (readTBMChan)
import Control.Concurrent (forkIO)

runApplication :: Config -> IO ()
runApplication c = do
  forkIO $ websocketConnection c
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
  get  "/status"       getStatusH
  get  "/utxos"        getUTXOsH

uiUpdater :: Config -> Connection -> IO ()
uiUpdater config wsConn = do
  muiUpdaterMessage <- atomically . readTBMChan $ config^.uiUpdaterChan
  case muiUpdaterMessage of
    Nothing -> fail "uiUpdaterChan is empty and closed"
    Just uiUpdaterMessage -> 
      case uiUpdaterMessage of
        IncomingFunds v -> handleIncomingFunds wsConn v
  uiUpdater config wsConn
  
websocketConnection :: Config -> IO ()
websocketConnection config = runServer "127.0.0.1" (config^.websocketPort) acceptAndRun
  where
    acceptAndRun pendingConnection = acceptRequest pendingConnection >>= uiUpdater config
