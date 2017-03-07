{-# LANGUAGE TemplateHaskell #-}
module Protocol.ConnectionM where

import Protocol.Network (Peer(..))
import Protocol.Messages (Message(..))
import General.Types ( HasVersion(..)
                     , HasRelay(..)
                     , HasTime(..)
                     , HasLastBlock(..)
                     , HasPeerAddr(..))
import General.Config (ConfigM(..), Config(..))
import General.Util (Addr(..))

import Data.Conduit.TMChan (TBMChan)
import Data.Time.Clock.POSIX (POSIXTime)
import System.Random (StdGen)
import Control.Lens (makeLenses)
import Control.Monad.State.Lazy (StateT(..))
import Control.Monad.Reader (runReaderT)
import Network.Socket (Socket)

data ConnectionContext = ConnectionContext
  { _connectionContextVersion :: Int
  , _connectionContextLastBlock :: Integer
  , _myAddr :: Addr
  --, _peer :: Peer
  , _connectionContextPeerAddr :: Addr
  , _connectionContextRelay :: Bool
    -- https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#extensions-to-existing-messages
    -- Relay should be set to False when functioning as an SPV node
  --, _writerChan :: TBMChan Message
  --, _listenChan :: TBMChan Message
  , _connectionContextTime :: POSIXTime
  , _randGen :: StdGen
  } 

makeLenses ''ConnectionContext

data IOHandlers = IOHandlers
  { _peerSocket :: Socket
  , _writerChan :: TBMChan Message
  , _listenChan :: TBMChan Message
  }

makeLenses ''IOHandlers

instance HasVersion ConnectionContext where
  version = connectionContextVersion

instance HasRelay ConnectionContext where
  relay = connectionContextRelay

instance HasTime ConnectionContext where
  time = connectionContextTime

instance HasLastBlock ConnectionContext where
  lastBlock = connectionContextLastBlock

instance HasPeerAddr ConnectionContext where
  peerAddr = connectionContextPeerAddr
