{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol.Network where

import General.Config (Config(..), configNetwork)
import General.Types (Network(..))
import General.Util (Addr(..))

import Network.Socket
  ( socket
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
import Control.Lens (makeLenses, (^.))
import Data.Binary (Binary(..))
import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import Data.ByteString.Base16 (decode)

data Peer = Peer
  { _sock :: Socket
  , _addr :: Addr
  } deriving (Show, Eq)

makeLenses ''Peer

connectToPeer :: Int -> Config -> IO Peer
connectToPeer n config = do
  let seed' = Just . seed $ config^.configNetwork
      port' = Just . networkPort $ config^.configNetwork
      isIPv4 addr = case addrAddress addr of
                          SockAddrInet _ _ -> True
                          _                -> False
  addrInfos <- filter isIPv4 <$> getAddrInfo Nothing seed' port'
  let addrInfo = addrInfos !! n
  --peerSocket <- socket AF_INET Stream 0
  peerSocket <- socket (addrFamily addrInfo) Stream defaultProtocol -- switch with previous line
    -- connect to local node: AF_INET Stream 0
  setSocketOption peerSocket KeepAlive 1
  --connect peerSocket (SockAddrInet 8333 0x0100007f)
  connect peerSocket (addrAddress addrInfo) -- switch with previous line
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
  Get.getWord64be -- services
  Get.getByteString 12 -- parse magic string
  a <- parseIpComponent
  b <- parseIpComponent
  c <- parseIpComponent
  d <- parseIpComponent
  port <- fromIntegral <$> Get.getWord16be
  return $ Addr (a, b, c, d) port
  where
    parseIpComponent = fromIntegral <$> Get.getWord8

putAddr :: Addr -> Put
putAddr (Addr (a, b, c, d) port) = do
  putServices
  Put.putByteString ipAddressMagicStr
  Put.putWord8 . fromIntegral $ a
  Put.putWord8 . fromIntegral $ b
  Put.putWord8 . fromIntegral $ c
  Put.putWord8 . fromIntegral $ d
  Put.putWord16be . fromIntegral $ port
  where
    ipAddressMagicStr = fst . decode $ "00000000000000000000FFFF"

putServices :: Put
putServices = Put.putWord64le 1

seed :: Network -> String
seed TestNet3 = "testnet-seed.bitcoin.petertodd.org"
seed MainNet = "seed.bitcoin.sipa.be"

networkPort :: Network -> String
networkPort TestNet3 = "18333"
networkPort MainNet = "8333"

