{-# LANGUAGE OverloadedStrings #-}

module Protocol.Parser where

import Protocol.Types (MessageBody(..), Header(..), readNetwork, readCommand, Network(..), Command(..), Addr(..), Message(..), getNetwork, MessageContext(..))
import qualified Data.ByteString.Char8 as Char8
import Data.Binary.Get (Get(..), getByteString, getWord32le, getWord64be, getWord64le, getWord8, getWord16be, isolate, bytesRead)
import Data.ByteString.Base16 (encode, decode)
import Foreign.Marshal.Utils (toBool)
import Data.ByteString (ByteString)
import Control.Monad (replicateM)
import BlockHeaders (BlockHash(..))
import BloomFilter (Filter(..), Tweak(..), NFlags(..))
import Data.Binary (Binary(..))
import Util (VarInt(..))

parseMessage :: Get Message
parseMessage = do
  network <- getNetwork
  command  <- readCommand . encode <$> getByteString 12
  messageLength <- fromIntegral <$> getWord32le
  checksum <- getWord32le
  messageBody <- isolate messageLength (parseMessageBody messageLength command)
  return $ Message messageBody (MessageContext network)
  
getPayload :: Get ByteString
getPayload = do
  length <- fromIntegral <$> getWord8
  getByteString length

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

parseRemaining :: Int -> Get ()
parseRemaining expectedLength = do
  read <- fromIntegral <$> bytesRead 
  getByteString (expectedLength - read)
  return ()

parseMessageBody :: Int -> Command -> Get MessageBody

parseMessageBody _ VersionCommand = do
  version     <- fromIntegral <$> getWord32le
  services    <- getWord64be
  timestamp   <- fromIntegral <$> getWord64le
  peer        <- getAddr
  sender      <- getAddr
  nonce       <- fromIntegral <$> getWord64be
  userAgent   <- getPayload
  startHeight <- fromIntegral <$> getWord32le
  relay       <- toBool <$> getWord8 
  return $ VersionMessage version nonce startHeight sender peer relay timestamp

-- These should all at least consume their length!
parseMessageBody expectedLength VerackCommand =
  parseRemaining expectedLength >> return VerackMessage
parseMessageBody expectedLength AddrCommand =
  parseRemaining expectedLength >> return AddrMessage
parseMessageBody expectedLength TxCommand =
  parseRemaining expectedLength >> return TxMessage
parseMessageBody expectedLength RejectCommand =
  parseRemaining expectedLength >> return RejectMessage
parseMessageBody expectedLength PingCommand =
  parseRemaining expectedLength >> return PingMessage
parseMessageBody expectedLength PongCommand =
  parseRemaining expectedLength >> return PongMessage
  
parseMessageBody _ InvCommand = do
   VarInt count <- get
   inventoryVectors <- replicateM count get
   return $ InvMessage inventoryVectors
  
parseMessageBody expectedLength GetDataCommand =
  parseRemaining expectedLength >> return GetDataMessage

parseMessageBody _ GetHeadersCommand = do
  version            <- fromIntegral <$> getWord32le
  VarInt nHashes     <- get
  blockLocatorHashes <- replicateM nHashes get
  hashStop           <- get
  return $ GetHeadersMessage version blockLocatorHashes hashStop
  
parseMessageBody expectedLength BlockCommand =
  parseRemaining expectedLength >> return BlockMessage
  
parseMessageBody expectedLength HeadersCommand = do
  VarInt nHeaders <- get
  blockHeaders    <- replicateM nHeaders get
  return $ HeadersMessage blockHeaders
  
parseMessageBody expectedLength GetAddrCommand =
  parseRemaining expectedLength >> return GetAddrMessage
parseMessageBody expectedLength MempoolCommand =
  parseRemaining expectedLength >> return MempoolMessage
parseMessageBody expectedLength CheckorderCommand =
  parseRemaining expectedLength >> return CheckorderMessage
parseMessageBody expectedLength SubmitorderCommand =
  parseRemaining expectedLength >> return SubmitorderMessage
  
parseMessageBody _ FilterloadCommand = do
  VarInt lengthFilter <- get
  filter <- Filter <$> getByteString lengthFilter
  nHashFuncs <- fromIntegral <$> getWord32le
  nTweak     <- Tweak . fromIntegral <$> getWord32le
  nFlags <- toEnum . fromIntegral <$> getWord8
  return $ FilterloadMessage filter nHashFuncs nTweak nFlags
  
parseMessageBody expectedLength FilteraddCommand =
  parseRemaining expectedLength >> return FilteraddMessage
parseMessageBody expectedLength FilterclearCommand =
  parseRemaining expectedLength >> return FilterclearMessage
parseMessageBody expectedLength MerkleblockCommand =
  parseRemaining expectedLength >> return MerkleblockMessage
parseMessageBody expectedLength SendheadersCommand =
  parseRemaining expectedLength >> return SendheadersMessage
parseMessageBody expectedLength FeefilterCommand =
  parseRemaining expectedLength >> return FeefilterMessage
parseMessageBody expectedLength SendcmpctCommand =
  parseRemaining expectedLength >> return SendcmpctMessage
parseMessageBody expectedLength CmpctblockCommand =
  parseRemaining expectedLength >> return CmpctblockMessage
parseMessageBody expectedLength GetblocktxnCommand =
  parseRemaining expectedLength >> return GetblocktxnMessage
parseMessageBody expectedLength BlocktxnCommand =
  parseRemaining expectedLength >> return BlocktxnMessage
parseMessageBody expectedLength UnknownCommand =
  parseRemaining expectedLength >> return UnknownMessage
