{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Protocol.Network where

import Protocol.Types (Addr(..))
import Protocol.Messages (getAddr)

import Network.Socket ( socket
                      , SocketType(..)
                      , defaultProtocol
                      , connect
                      , getAddrInfo
                      , AddrInfo(..)
                      , setSocketOption
                      , SocketOption(..)
                      , Socket)
import Control.Lens (makeFields)


data Peer = Peer
  { _peerSock :: Socket
  , _peerAddr :: Addr
  } deriving (Show, Eq)

makeFields ''Peer

connectToPeer :: Int -> IO Peer
connectToPeer n = do
  addrInfo <- (!! n) <$> getAddrInfo Nothing (Just "testnet-seed.bitcoin.petertodd.org") (Just "18333")
  peerSocket <- socket (addrFamily addrInfo) Stream defaultProtocol
  setSocketOption peerSocket KeepAlive 1
  connect peerSocket (addrAddress addrInfo)
  return $ Peer peerSocket (getAddr $ addrAddress addrInfo)
