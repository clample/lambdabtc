{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Keys
import Data.Base58String.Bitcoin (toBytes, toText)
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
import Network.Wai.Handler.Warp (Settings, setPort, defaultSettings)
import Data.Default (def)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.HTTP.Types.Status (internalServerError500, ok200)
import Data.Aeson (object, (.=), Value (Null))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text.Lazy as LT


data Config =
  Config { environment :: Environment
         , port :: Int }

developmentConfig :: Config
developmentConfig =
  Config Development 49535

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


main :: IO ()
main = runApplication developmentConfig

createAndShowKey = do
  (pubKey, privKey) <- genKeys
  let WIF b58wifPrivKey = getWIFPrivateKey $ getHexPrivateKey privKey
  putStrLn $ unpack  . toText $ b58wifPrivKey

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
  post "/fundrequests" $ do
    status ok200

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

defaultH :: Environment -> Error -> Action
defaultH e x = do
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o
