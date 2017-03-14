{-# LANGUAGE TemplateHaskell #-}
module Protocol.ConnectionM where

import Protocol.Messages (Message(..))
import General.Types ( HasVersion(..)
                     , HasRelay(..)
                     , HasTime(..)
                     , HasPeerAddr(..)
                     , Network(..)
                     , HasPool(..)
                     , HasNetwork(..))
import General.Config (HasAppChan(..), HasUIUpdaterChan(..))
import General.Util (Addr(..))
import General.InternalMessaging (UIUpdaterMessage(..), InternalMessage(..))
import Protocol.Util (HasLastBlock(..), BlockIndex(..))

import Data.Conduit.TMChan (TBMChan)
import Data.Time.Clock.POSIX (POSIXTime)
import System.Random (StdGen)
import Control.Lens (makeLenses)
import Network.Socket (Socket)
import Database.Persist.Sql (ConnectionPool)
import BitcoinCore.BlockHeaders (BlockHeader)

data ConnectionContext = ConnectionContext
  { _connectionContextVersion :: Int
  , _connectionContextLastBlock :: BlockIndex
  , _myAddr :: Addr
  , _connectionContextPeerAddr :: Addr
  , _connectionContextRelay :: Bool
    -- https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#extensions-to-existing-messages
    -- Relay should be set to False when functioning as an SPV node
  , _connectionContextTime :: POSIXTime
  , _randGen :: StdGen
  , _connectionContextNetwork :: Network
  , _rejectedBlocks :: [BlockHeader]  
  } 

makeLenses ''ConnectionContext

data IOHandlers = IOHandlers
  { _peerSocket :: Socket
  , _writerChan :: TBMChan Message
  , _listenChan :: TBMChan Message
  , _ioHandlersUIUpdaterChan :: TBMChan UIUpdaterMessage
  , _ioHandlersAppChan :: TBMChan InternalMessage
  , _ioHandlersPool :: ConnectionPool
  }

makeLenses ''IOHandlers

instance HasPool IOHandlers where
  pool = ioHandlersPool

instance HasUIUpdaterChan IOHandlers where
  uiUpdaterChan = ioHandlersUIUpdaterChan

instance HasAppChan IOHandlers where
  appChan = ioHandlersAppChan

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

instance HasNetwork ConnectionContext where
  network = connectionContextNetwork
