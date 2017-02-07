module Protocol.Network where

import Protocol.Types (Addr(..))
import Protocol.Messages (getAddr)

import Network.Socket ( socket
                      , Family(..)
                      , SocketType(..)
                      , defaultProtocol
                      , connect
                      , SockAddr(..)
                      , tupleToHostAddress
                      , getAddrInfo
                      , AddrInfo(..)
                      , iNADDR_ANY
                      , defaultHints
                      , AddrInfoFlag(..)
                      , setSocketOption
                      , SocketOption(..)
                      , Socket)

data Peer = Peer Socket Addr
  deriving (Show, Eq)

connectToPeer :: Int -> IO (Peer)
connectToPeer n = do
  addrInfo <- (!! n) <$> getAddrInfo Nothing (Just "testnet-seed.bitcoin.petertodd.org") (Just "18333")
  peerSocket <- socket (addrFamily addrInfo) Stream defaultProtocol
  setSocketOption peerSocket KeepAlive 1
  connect peerSocket (addrAddress addrInfo)
  return $ Peer peerSocket (getAddr $ addrAddress addrInfo)
