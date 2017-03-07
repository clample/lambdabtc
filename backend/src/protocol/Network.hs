{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol.Network where

import General.Config (Config(..), configNetwork)
import General.Types (Network(..))

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
                      , Family(..)
                      )
import Control.Lens (makeFields, (^.))
import Data.Binary (Binary(..))
import Data.Binary.Get (Get, getWord64be, getWord16be, getByteString, getWord8)
import Data.Binary.Put (Put, putByteString, putWord8, putWord16be,  putWord64le)
import Data.ByteString.Base16 (decode)

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose)

data Peer = Peer
  { _peerSock :: Socket
  , _peerAddr :: Addr
  } deriving (Show, Eq)

data Addr = Addr IP Port
  deriving (Show, Eq)

type IP = (Int, Int, Int, Int)

type Port = Int


makeFields ''Peer

connectToPeer :: Int -> Config -> IO Peer
connectToPeer n config = do
  let seed' = Just . seed $ config^.configNetwork
      port' = Just . networkPort $ config^.configNetwork
  addrInfo <- (!! n) <$> getAddrInfo Nothing seed' port'
  peerSocket <- socket AF_INET Stream 0 -- (addrFamily addrInfo) Stream defaultProtocol
    -- connect to local node: AF_INET Stream 0
  setSocketOption peerSocket KeepAlive 1
  connect peerSocket (SockAddrInet 8333 0x0100007f) -- (addrAddress addrInfo)
    -- connect to local node: (SockAddrInet 8333 0x0100007f)
    -- connect to local node
  return $ Peer peerSocket (addrFromSock $ addrAddress addrInfo)

addrFromSock :: SockAddr -> Addr
addrFromSock (SockAddrInet port host) =
  Addr hostIP (fromIntegral port)
  where
    hostIP = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)
    (a, b, c, d) = hostAddressToTuple host
addrFromSock sockAddr = error
  $  "Error getting address from sock "
  ++ show sockAddr
  ++ " It is currentently only possible to get address from SockAddrInet."

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

seed :: Network -> String
seed TestNet3 = "testnet-seed.bitcoin.petertodd.org"
seed MainNet = "seed.bitcoin.sipa.be"

networkPort :: Network -> String
networkPort TestNet3 = "18333"
networkPort MainNet = "8333"

instance Arbitrary Addr where
  arbitrary = do
    a <- chooseIpComponent
    b <- chooseIpComponent
    c <- chooseIpComponent
    d <- chooseIpComponent
    port <- choosePort
    return $ Addr (a, b, c, d) port
    where
      chooseIpComponent = choose (0, 255)
      choosePort = choose (0, 65535)
