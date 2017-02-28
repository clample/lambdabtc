{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module General.Config where

import General.Types (HasNetwork(..), Network(..))
import General.InternalMessaging ( InternalMessage(..)
                                 , UIUpdaterMessage(..))

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Web.Scotty.Trans (ActionT, Options(..))
import qualified Data.Text.Lazy as LT
import Data.Text (Text)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Handler.Warp (Settings, setPort, defaultSettings)
import Data.Default (def)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite (createSqlitePool)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Lens (makeLenses, (^.))
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChan)
import GHC.Conc (atomically)

data Environment =
  Development |
  Production |
  Test
  deriving (Eq, Read, Show)

data Config =
  Config { _environment :: Environment
         , _port :: Int 
         , _websocketPort :: Int
         , _pool :: ConnectionPool
         , _configNetwork :: Network
         , _appChan :: TBMChan InternalMessage
         , _uiUpdaterChan :: TBMChan UIUpdaterMessage
         }

makeLenses ''Config

instance HasNetwork Config where
  network = configNetwork

developmentConfig :: IO Config
developmentConfig = do
  let env = Development
      network' = MainNet 
  pool' <- runStdoutLoggingT $
    createSqlitePool (dbFile network') (getConnectionSize env)
  chan <- atomically $ newTBMChan 16
  uiUpdaterChan' <- atomically $ newTBMChan 16
  return $ Config
    { _environment = Development
    , _port = 49535
    , _websocketPort = 49536
    , _pool = pool'
    , _configNetwork = network'
    , _appChan = chan
    , _uiUpdaterChan = uiUpdaterChan'
    }

getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production = 8
getConnectionSize Test = 1

dbFile :: Network -> Text
dbFile TestNet3 = "file:resources/sqlite3-testnet3.db"
dbFile MainNet = "file:resources/sqlite3-mainnet.db"

type Error = LT.Text

newtype ConfigM a =
  ConfigM { runConfigM :: ReaderT Config IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Action = ActionT Error ConfigM ()

getOptions :: Config -> Options
getOptions config =
  def { settings = getSettings
      , verbose = case (config^.environment) of
                    Development -> 1
                    Production -> 0
                    Test -> 0
      }
  
-- TODO: Port is duplicated between here and developmentConfig
getSettings :: Settings
getSettings = 
  setPort 49535 defaultSettings

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout
loggingM Test = id
