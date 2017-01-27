{-# LANGUAGE OverloadedStrings #-}

module Messages where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as T
import Util (switchEndian, hexify, payloadLength', checkSum, showBool)
import Data.ByteString.Base16 (decode, encode)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Lens (over, mapped)
import Data.Maybe (fromJust)
import Data.List (lookup)
import Data.Tuple (swap)
import Transaction (Transaction(..), signedTransaction)
import Network.Socket (SockAddr(..), hostAddressToTuple)
import Text.Megaparsec (Parsec, Dec)
import Data.Char (toUpper)
import Control.Lens (over, _2, mapped)
import Protocol.Types

showMessage :: Message -> ByteString
showMessage message@(Message  messageBody context) =
  headerBS `BS.append` messageBS
  where
    headerBS = showHeader $ Header (network context) (getCommand messageBody) messageBS
    messageBS = showMessageBody message

showMessageBody :: Message -> ByteString

showMessageBody (Message (VersionMessage v randInt blockN senderAddr peerAddr relay) context) = BS.concat
  [ showVersion v
  , services
  , timestamp
  , addrRecv
  , addrFrom
  , nonce
  , userAgent
  , startHeight
  , showBool relay ]
  where
    timestamp = (switchEndian . T.encodeUtf8 . flip hexify 16 . floor) (time context)
    addrRecv = networkAddress peerAddr
    addrFrom = networkAddress senderAddr
    nonce = (BS.take 16 . T.encodeUtf8 . flip hexify 16 . fromIntegral) randInt
    userAgent = "00" -- See https://github.com/bitcoin/bips/blob/master/bip-0014.mediawiki
    startHeight = (switchEndian . T.encodeUtf8 . flip hexify 8 . fromIntegral) blockN

showMessageBody _ = ""

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

-- Get rid of Header type and make this Message -> MessageBody -> ByteString
-- type MessageBody = ByteString
showHeader :: Header -> ByteString
showHeader (Header network command message) = BS.concat
  [ printNetwork network
  , printCommand command
  , switchEndian $ payloadLength' 8 message
  , (encode . checkSum . fst . decode) message ]

showVersion :: Int -> ByteString
showVersion version = switchEndian . T.encodeUtf8 $ hexify (fromIntegral version) 8

services :: ByteString -- This should not be hardcoded
services = "0100000000000000"

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
  
