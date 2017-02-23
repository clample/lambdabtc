{-# LANGUAGE TemplateHaskell #-}
module Protocol.ConnectionM where

import Protocol.Network (Addr(..), Peer(..))
import Protocol.Messages (Message(..))
import General.Types ( HasVersion(..)
                     , HasRelay(..)
                     , HasTime(..)
                     , HasLastBlock(..))
import General.Config (ConfigM(..), Config(..))

import Data.Conduit.TMChan (TBMChan)
import Data.Time.Clock.POSIX (POSIXTime)
import System.Random (StdGen)
import Control.Lens (makeLenses)
import Control.Monad.State.Lazy (StateT(..))
import Control.Monad.Reader (runReaderT)

data ConnectionContext = ConnectionContext
  { _connectionContextVersion :: Int
  , _connectionContextLastBlock :: Integer
  , _myAddr :: Addr
  , _peer :: Peer
  , _connectionContextRelay :: Bool
    -- https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#extensions-to-existing-messages
    -- Relay should be set to False when functioning as an SPV node
  , _writerChan :: TBMChan Message
  , _listenChan :: TBMChan Message
  , _connectionContextTime :: POSIXTime
  , _randGen :: StdGen
  } 

makeLenses ''ConnectionContext

instance HasVersion ConnectionContext where
  version = connectionContextVersion

instance HasRelay ConnectionContext where
  relay = connectionContextRelay

instance HasTime ConnectionContext where
  time = connectionContextTime

instance HasLastBlock ConnectionContext where
  lastBlock = connectionContextLastBlock

type Connection a = StateT ConnectionContext ConfigM a

runConnection :: Connection a -> ConnectionContext -> Config -> IO (a, ConnectionContext)
runConnection connectionM state =
  runReaderT (runConfigM (runStateT connectionM state))
