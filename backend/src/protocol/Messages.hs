{-# LANGUAGE OverloadedStrings #-}

module Protocol.Messages where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as T
import Util (checkSum, VarInt(..))
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
import Protocol.Types (getCommand', getNetwork', Network(..), Addr(..), Header(..), Command(..), Message(..), MessageContext(..), getCommand, MessageBody (..))
import Data.Binary.Put (Put, putWord16be, putWord32le, putWord32be, putWord64le, putWord64be, putWord8, putByteString, runPut)
import Data.Binary (Binary(..))
import BloomFilter (Filter(..), Tweak(..))
import qualified Data.ByteString.Lazy as BL

putMessage :: Message -> Put
putMessage message@(Message messageBody context) = do
  putHeader (Header
              (network context)
              (getCommand messageBody)
              (BL.toStrict $ messageBS))
  messageBodyEncode
  where
    messageBS = runPut messageBodyEncode
    messageBodyEncode = putMessageBody messageBody

putHeader :: Header -> Put
putHeader (Header network command message) = do
  putByteString $ getNetwork' network
  putByteString $ getCommand' command
  putWord32le $ fromIntegral (BS.length message)
  putByteString $ checkSum message

putMessageBody :: MessageBody -> Put

putMessageBody (VersionMessage version randInt blockN senderAddr peerAddr relay time) = do
  putWord32le (fromIntegral version)
  putServices
  putWord64le . floor $ time
  putAddr peerAddr
  putAddr senderAddr
  putWord64be . fromIntegral $ randInt
  putWord8 0
  putWord32le . fromIntegral $ blockN
  put relay

putMessageBody (GetHeadersMessage version blockLocatorHashes hashStop) = do
  putWord32le (fromIntegral version)
  put (VarInt . length $ blockLocatorHashes)
  mapM_ put blockLocatorHashes
  put hashStop

putMessageBody (HeadersMessage blockHeaders) = do
  put (VarInt . length $ blockHeaders)
  mapM_ put blockHeaders

putMessageBody (FilterloadMessage (Filter filter) nHashFuncs (Tweak nTweak) nFlags) = do
  put . VarInt . BS.length $ filter
  putByteString filter
  putWord32le (fromIntegral nHashFuncs)
  putWord32le (fromIntegral nTweak)
    -- TODO: It's not clear if nTweak should be litle or big endian
    --       This won't be a problem when nTweak is 0, but it may cause bugs later
  (putWord8 . fromIntegral . fromEnum) nFlags

putMessageBody (InvMessage invVectors) = do
  put . VarInt . length $ invVectors
  mapM_ put invVectors

putMessageBody _ = putByteString ""

putServices :: Put
putServices = putWord64le 1

getAddr :: SockAddr -> Addr
getAddr (SockAddrInet port host) =
  Addr hostIP (fromIntegral port)
  where
    hostIP = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)
    (a, b, c, d) = hostAddressToTuple host

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


