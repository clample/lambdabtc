{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module General.Config where

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Web.Scotty.Trans (ActionT, Options(..))
import qualified Data.Text.Lazy as LT
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Handler.Warp (Settings, setPort, defaultSettings)
import Data.Default (def)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Sqlite (createSqlitePool)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Lens (makeFields, (^.))


data Environment =
  Development |
  Production |
  Test
  deriving (Eq, Read, Show)

data Config =
  Config { _configEnvironment :: Environment
         , _configPort :: Int -- port might belong elsewhere
         , _configPool :: ConnectionPool}

makeFields ''Config

developmentConfig :: IO Config
developmentConfig = do
  let env = Development
  pool' <- runStdoutLoggingT $
    createSqlitePool "file:resources/sqlite3.db" (getConnectionSize env)
  return $ Config Development 49535 pool'

getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production = 8
getConnectionSize Test = 1

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
  

getSettings :: Settings
getSettings = 
  setPort 49535 defaultSettings

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout
loggingM Test = id

