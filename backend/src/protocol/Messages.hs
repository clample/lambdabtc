{-# LANGUAGE OverloadedStrings #-}

module Messages where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as T
import Util (switchEndian, hexify, payloadLength', checkSum)
import Data.ByteString.Base16 (decode, encode)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Lens (over, mapped)
import Data.Maybe (fromJust)
import Data.List (lookup)
import Data.Tuple (swap)
import Transaction (Transaction(..), signedTransaction)
import Network.Socket (SockAddr(..), hostAddressToTuple)

data VersionMessage = VersionMessage
  { version    :: Int
  , network    :: Network
  , time       :: POSIXTime
  , nonceInt   :: Integer -- Nonce can be 8 bytes -> use integer 
  , lastBlockN :: Integer
  , senderAddr :: Addr
  , peerAddr   :: Addr
  } deriving (Show, Eq)
  

showVersionMessage :: VersionMessage -> ByteString
showVersionMessage (VersionMessage v network time randInt blockN senderAddr peerAddr) =
  headerBS  `BS.append` message
  where
    headerBS = showHeader $ Header network VersionCommand message
    message = BS.concat
      [ showVersion v
      , services
      , timestamp
      , addrRecv
      , addrFrom
      , nonce
      , userAgent
      , startHeight
      , relay ]
    timestamp = (switchEndian . T.encodeUtf8 . flip hexify 16 . floor) time
    addrRecv = networkAddress peerAddr
    addrFrom = networkAddress senderAddr
    nonce = (BS.take 16 . T.encodeUtf8 . flip hexify 16 . fromIntegral) randInt
    userAgent = "00" -- See https://github.com/bitcoin/bips/blob/master/bip-0014.mediawiki
    startHeight = (switchEndian . T.encodeUtf8 . flip hexify 8 . fromIntegral) blockN
    relay = "" -- See https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki

verackMessage :: Network -> ByteString
verackMessage network =
  showHeader $ Header network VerackCommand ""

addrMessage :: Network -> POSIXTime -> Addr -> ByteString
addrMessage network time addr =
  headerBS `BS.append` message
  where
    headerBS = showHeader $ Header network AddrCommand message
    message = networkAddress addr

txMessage :: Network -> Transaction -> ByteString
txMessage network transaction =
  headerBS `BS.append` message
  where
    headerBS = showHeader $ Header network TxCommand message
    message = signedTransaction transaction

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

data Command
  = VersionCommand
  | VerackCommand
  | AddrCommand
  | TxCommand
  | RejectCommand
  deriving (Show, Eq)

commandTable :: [(Command, ByteString)]
commandTable =
  [ (VersionCommand, "76657273696F6E0000000000")
  , (VerackCommand , "76657261636B000000000000")
  , (AddrCommand,    "616464720000000000000000")
  , (TxCommand,      "747800000000000000000000")
  , (RejectCommand,  "72656a656374000000000000")]

printCommand :: Command -> ByteString
printCommand = fromJust . flip lookup commandTable

readCommand :: ByteString -> Maybe Command
readCommand = flip lookup (map swap commandTable)
-------------------------------

showVersion :: Int -> ByteString
showVersion version = switchEndian . T.encodeUtf8 $ hexify (fromIntegral version) 8

services :: ByteString -- This should not be hardcoded
services = "0100000000000000"

data Addr = Addr IP Port
  deriving (Show, Eq)

type IP = (Int, Int, Int, Int)

type Port = Int

getAddr :: SockAddr -> Addr
getAddr (SockAddrInet port host) =
  Addr hostIP (fromIntegral port)
  where
    hostIP = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)
    (a, b, c, d) = hostAddressToTuple host

networkAddress :: Addr -> ByteString
networkAddress (Addr (a, b, c, d) port) =
  BS.concat [services, ipAddress, portBS]
  where
    portBS = (T.encodeUtf8 . flip hexify 4 . fromIntegral) port
    ipAddress =  BS.concat
                 [ ipAddressMagicStr
                 , showAddressComponent a
                 , showAddressComponent b
                 , showAddressComponent c
                 , showAddressComponent d]
    ipAddressMagicStr = "00000000000000000000FFFF"  
    showAddressComponent = T.encodeUtf8 . flip hexify 2 . fromIntegral
  
