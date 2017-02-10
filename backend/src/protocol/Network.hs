{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol.Network where

import Network.Socket ( socket
                      , SocketType(..)
                      , defaultProtocol
                      , connect
                      , getAddrInfo
                      , AddrInfo(..)
                      , setSocketOption
                      , SocketOption(..)
                      , Socket
                      , SockAddr(..)
                      , hostAddressToTuple
                      )
import Control.Lens (makeFields)
import Data.Binary (Binary(..))
import Data.Binary.Get (Get, getWord64be, getWord16be, getByteString, getWord8)
import Data.Binary.Put (Put, putByteString, putWord8, putWord16be,  putWord64le)
import Data.ByteString.Base16 (decode)

data Peer = Peer
  { _peerSock :: Socket
  , _peerAddr :: Addr
  } deriving (Show, Eq)

data Addr = Addr IP Port
  deriving (Show, Eq)

type IP = (Int, Int, Int, Int)

type Port = Int


makeFields ''Peer

connectToPeer :: Int -> IO Peer
connectToPeer n = do
  addrInfo <- (!! n) <$> getAddrInfo Nothing (Just "testnet-seed.bitcoin.petertodd.org") (Just "18333")
  peerSocket <- socket (addrFamily addrInfo) Stream defaultProtocol
  setSocketOption peerSocket KeepAlive 1
  connect peerSocket (addrAddress addrInfo)
  return $ Peer peerSocket (addrFromSock $ addrAddress addrInfo)

addrFromSock :: SockAddr -> Addr
addrFromSock (SockAddrInet port host) =
  Addr hostIP (fromIntegral port)
  where
    hostIP = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)
    (a, b, c, d) = hostAddressToTuple host

---------------------------------

instance Binary Addr where
  put = putAddr
  get = getAddr

getAddr :: Get Addr
getAddr = do
  getWord64be -- services
  getByteString 12 -- parse magic string
  a <- parseIpComponent
  b <- parseIpComponent
  c <- parseIpComponent
  d <- parseIpComponent
  port <- fromIntegral <$> getWord16be
  return $ Addr (a, b, c, d) port
  where
    parseIpComponent = fromIntegral <$> getWord8

putAddr :: Addr -> Put
putAddr (Addr (a, b, c, d) port) = do
  putServices
  putByteString ipAddressMagicStr
  putWord8 . fromIntegral $ a
  putWord8 . fromIntegral $ b
  putWord8 . fromIntegral $ c
  putWord8 . fromIntegral $ d
  putWord16be . fromIntegral $ port
  where
    ipAddressMagicStr = fst . decode $ "00000000000000000000FFFF"

putServices :: Put
putServices = putWord64le 1


