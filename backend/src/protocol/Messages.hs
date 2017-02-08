{-# LANGUAGE OverloadedStrings #-}

module Protocol.Messages where

import Util (checkSum, VarInt(..))
import BitcoinCore.BloomFilter (Filter(..), Tweak(..), serializeFilter)
import Protocol.Types ( getCommand'
                      , getNetwork'
                      , Addr(..)
                      , Header(..)
                      , Message(..)
                      , getCommand
                      , MessageBody (..)
                      , network
                      , VersionMessage(..)
                      , GetHeadersMessage(..)
                      , HeadersMessage(..)
                      , FilterloadMessage(..)
                      , InvMessage(..))


import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decode)
import Network.Socket (SockAddr(..), hostAddressToTuple)
import Data.Binary.Put (Put, putWord16be, putWord32le, putWord64le, putWord64be, putWord8, putByteString, runPut)
import Data.Binary (Binary(..))
import qualified Data.ByteString.Lazy as BL
import Control.Lens ((^.))

putMessage :: Message -> Put
putMessage message@(Message messageBody context) = do
  putHeader (Header
              (context^.network)
              (getCommand messageBody)
              (BL.toStrict messageBS))
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

putMessageBody (VersionMessageBody (VersionMessage version randInt blockN senderAddr peerAddr relay time)) = do
  putWord32le (fromIntegral version)
  putServices
  putWord64le . floor $ time
  putAddr peerAddr
  putAddr senderAddr
  putWord64be . fromIntegral $ randInt
  putWord8 0
  putWord32le . fromIntegral $ blockN
  put relay

putMessageBody (GetHeadersMessageBody (GetHeadersMessage version blockLocatorHashes hashStop)) = do
  putWord32le (fromIntegral version)
  put (VarInt . length $ blockLocatorHashes)
  mapM_ put blockLocatorHashes
  put hashStop

putMessageBody (HeadersMessageBody (HeadersMessage blockHeaders)) = do
  put (VarInt . length $ blockHeaders)
  mapM_ put blockHeaders

putMessageBody (FilterloadMessageBody (FilterloadMessage filter nHashFuncs (Tweak nTweak) nFlags)) = do
  put . VarInt . filterLengthBytes $ filter
  putByteString . serializeFilter $ filter
  putWord32le (fromIntegral nHashFuncs)
  putWord32le (fromIntegral nTweak)
    -- TODO: It's not clear if nTweak should be litle or big endian
    --       This won't be a problem when nTweak is 0, but it may cause bugs later
  (putWord8 . fromIntegral . fromEnum) nFlags

putMessageBody (InvMessageBody (InvMessage invVectors)) = do
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


