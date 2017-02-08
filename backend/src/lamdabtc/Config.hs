{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LamdaBTC.Config where

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


data Config =
  Config { environment :: Environment
         , port :: Int -- port might belong elsewhere
         , pool :: ConnectionPool}

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

data Environment =
  Development |
  Production |
  Test
  deriving (Eq, Read, Show)

type Error = LT.Text

newtype ConfigM a =
  ConfigM { runConfigM :: ReaderT Config IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Action = ActionT Error ConfigM ()

getOptions :: Config -> Options
getOptions config =
  def { settings = getSettings
      , verbose = case environment config of
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

