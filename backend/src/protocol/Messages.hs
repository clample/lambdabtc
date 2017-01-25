{-# LANGUAGE OverloadedStrings #-}

module Messages where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as T
import Util (switchEndian, hexify, payloadLength', checkSum)
import Data.ByteString.Base16 (decode, encode)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Network.Socket (connect
                      , socket
                      , Family(..)
                      , SocketType(..)
                      , defaultProtocol
                      , connect
                      , SockAddr(..)
                      , tupleToHostAddress
                      , getAddrInfo
                      , AddrInfo(..)
                      , iNADDR_ANY
                      , bind
                      , defaultHints
                      , AddrInfoFlag(..)
                      , setSocketOption
                      , SocketOption(..)
                      , hostAddressToTuple)
import Network.Socket.ByteString (send, recv )
import Control.Lens (over, mapped)
import Data.Maybe (fromJust)
import Data.List (lookup)
import Data.Tuple (swap)

versionMessage :: Network -> POSIXTime -> Int -> Int -> SockAddr -> ByteString
versionMessage network time randInt blockN peerAddr =
  headerBS  `BS.append` message
  where
    headerBS = showHeader $ Header network VersionCommand message
    message = BS.concat
      [ version 60002
      , services
      , timestamp
      , addrRecv
      , addrFrom
      , nonce
      , userAgent
      , startHeight
      , relay ]
    timestamp = (switchEndian . T.encodeUtf8 . flip hexify 16 . floor) time
    addrRecv = networkAddress' peerAddr
    addrFrom = networkAddress Nothing (207, 251, 103, 46 ) 18333
    nonce = (BS.take 16 . T.encodeUtf8 . flip hexify 16 . fromIntegral) randInt
      -- Maybe this needs to be made more carefully / cryptographically secure?
    userAgent = "00" -- See https://github.com/bitcoin/bips/blob/master/bip-0014.mediawiki
    startHeight = (switchEndian . T.encodeUtf8 . flip hexify 8 . fromIntegral) blockN
    relay = "" -- See https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki

showHeader :: Header -> ByteString
showHeader (Header network command message) = BS.concat
  [ printNetwork network
  , printCommand command
  , switchEndian $ payloadLength' 8 message
  , (encode . checkSum . fst . decode) message ]

data Header = Header Network Command ByteString
  deriving (Show, Eq)

-------------------------------
data Network = TestNet3 | MainNet
  deriving (Show, Eq)

networkTable :: [(Network, ByteString)]
networkTable =
  [ (TestNet3, "0B110907")
  , (MainNet,  "F9BEB4D9")]

printNetwork :: Network -> ByteString
printNetwork = fromJust . flip lookup networkTable

readNetwork :: ByteString -> Maybe Network
readNetwork = flip lookup (map swap networkTable)
-------------------------------

data Command = VersionCommand
  deriving (Show, Eq)

commandTable :: [(Command, ByteString)]
commandTable =
  [ (VersionCommand, "76657273696F6E0000000000")]

printCommand :: Command -> ByteString
printCommand = fromJust . flip lookup commandTable

readCommand :: ByteString -> Maybe Command
readCommand = flip lookup (map swap commandTable)
-------------------------------

version :: Int -> ByteString
version v = switchEndian . T.encodeUtf8 $ hexify (fromIntegral v) 8

services :: ByteString -- This should not be hardcoded
services = "0100000000000000"

networkAddress :: (Maybe POSIXTime) -> (Int, Int, Int, Int) -> Int -> ByteString
networkAddress mTime (a, b, c, d) port =
  case mTime of
    Nothing -> BS.concat [services, ipAddress, portBS]
    Just time -> BS.concat [timestamp time, services, ipAddress, portBS]
  where
    portBS = (T.encodeUtf8 . flip hexify 4 . fromIntegral) port
    timestamp = (switchEndian . T.encodeUtf8 . flip hexify 8 . floor)
    ipAddress =  BS.concat
                 [ ipAddressMagicStr
                 , showAddressComponent a
                 , showAddressComponent b
                 , showAddressComponent c
                 , showAddressComponent d]
    ipAddressMagicStr = "00000000000000000000FFFF"  
    showAddressComponent = T.encodeUtf8 . flip hexify 2 . fromIntegral
    -- time is not present in version message. It is present in other messages

networkAddress' :: SockAddr -> ByteString
networkAddress' (SockAddrInet port host) =
  networkAddress Nothing hostIP (fromIntegral port)
  where
    (a, b, c, d) = hostAddressToTuple host
    hostIP = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)
    

----------------------
-- Find testnet hosts with `nslookup testnet-seed.bitcoin.petertodd.org`

connectTestnet = do
  addrInfo <- (head) <$> getAddrInfo Nothing (Just "testnet-seed.bitcoin.petertodd.org") (Just "18333")
  peerSocket <- socket (addrFamily addrInfo) Stream defaultProtocol
  setSocketOption peerSocket KeepAlive 1
  connect peerSocket (addrAddress addrInfo)
  putStrLn "Great Job, we connected"
  time <- getPOSIXTime
  let message = versionMessage TestNet3 time 100 10 (addrAddress addrInfo)
  send peerSocket $ fst . decode $ message
  putStrLn . Char8.unpack $ message
  bs <- recv peerSocket 300
  putStrLn $ "Recieved message: " ++ (Char8.unpack . encode) bs

--------------- Example

{--
printVersionMessage :: IO ()
printVersionMessage = do
  time <- getPOSIXTime
  putStrLn . Char8.unpack $ versionMessage time 100 1
--}

headerCheck :: String
headerCheck = Char8.unpack $ showHeader $ Header TestNet3 VersionCommand
  "62EA0000010000000000000011B2D05000000000010000000000000000000000000000000000FFFF000000000000010000000000000000000000000000000000FFFF0000000000003B2EB35D8CE617650F2F5361746F7368693A302E372E322FC03E0300"

exampleAddress :: String
exampleAddress = Char8.unpack $ networkAddress Nothing (10, 0, 0, 1) 8333
