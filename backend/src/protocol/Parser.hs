{-# LANGUAGE OverloadedStrings #-}

module Protocol.Parser where

import Protocol.Types 
import BitcoinCore.BloomFilter (Tweak(..), deserializeFilter)
import General.Util (VarInt(..))

import Data.Binary.Get (Get, getByteString, getWord32le, getWord64be, getWord64le, getWord8, getWord16be, isolate, bytesRead)
import Data.ByteString.Base16 (encode)
import Foreign.Marshal.Utils (toBool)
import Data.ByteString (ByteString)
import Control.Monad (replicateM)
import Data.Binary (Binary(..))

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
  return $ VersionMessageBody
    (VersionMessage version nonce startHeight sender peer relay timestamp)

-- These should all at least consume their length!
parseMessageBody expectedLength VerackCommand =
  parseRemaining expectedLength >> return
    (VerackMessageBody VerackMessage)
parseMessageBody expectedLength AddrCommand =
  parseRemaining expectedLength >> return
    (AddrMessageBody AddrMessage)
parseMessageBody expectedLength TxCommand =
  parseRemaining expectedLength >> return
    (TxMessageBody TxMessage)
parseMessageBody expectedLength RejectCommand =
  parseRemaining expectedLength >> return
    (RejectMessageBody RejectMessage)
parseMessageBody expectedLength PingCommand =
  parseRemaining expectedLength >> return
    (PingMessageBody PingMessage)
parseMessageBody expectedLength PongCommand =
  parseRemaining expectedLength >> return
    (PongMessageBody PongMessage)
  
parseMessageBody _ InvCommand = do
   VarInt count <- get
   inventoryVectors <- replicateM count get
   return $ InvMessageBody (InvMessage inventoryVectors)
  
parseMessageBody expectedLength GetDataCommand =
  parseRemaining expectedLength >> return
    (GetDataMessageBody GetDataMessage)

parseMessageBody _ GetHeadersCommand = do
  version            <- fromIntegral <$> getWord32le
  VarInt nHashes     <- get
  blockLocatorHashes <- replicateM nHashes get
  hashStop           <- get
  return $ GetHeadersMessageBody
    (GetHeadersMessage version blockLocatorHashes hashStop)
  
parseMessageBody expectedLength BlockCommand =
  parseRemaining expectedLength >> return
    (BlockMessageBody BlockMessage)
  
parseMessageBody expectedLength HeadersCommand = do
  VarInt nHeaders <- get
  blockHeaders    <- replicateM nHeaders get
  return $ HeadersMessageBody (HeadersMessage blockHeaders)
  
parseMessageBody expectedLength GetAddrCommand =
  parseRemaining expectedLength >> return
    (GetAddrMessageBody GetAddrMessage)
parseMessageBody expectedLength MempoolCommand =
  parseRemaining expectedLength >> return
    (MempoolMessageBody MempoolMessage)
parseMessageBody expectedLength CheckorderCommand =
  parseRemaining expectedLength >> return
    (CheckorderMessageBody CheckorderMessage)
parseMessageBody expectedLength SubmitorderCommand =
  parseRemaining expectedLength >> return
    (SubmitorderMessageBody SubmitorderMessage)
  
parseMessageBody _ FilterloadCommand = do
  VarInt lengthFilter <- get
  filter <- deserializeFilter <$> getByteString lengthFilter
  nHashFuncs <- fromIntegral <$> getWord32le
  nTweak     <- Tweak . fromIntegral <$> getWord32le
  nFlags <- toEnum . fromIntegral <$> getWord8
  return $ FilterloadMessageBody
    (FilterloadMessage filter nHashFuncs nTweak nFlags)
  
parseMessageBody expectedLength FilteraddCommand =
  parseRemaining expectedLength >> return
    (FilteraddMessageBody FilteraddMessage)
parseMessageBody expectedLength FilterclearCommand =
  parseRemaining expectedLength >> return
    (FilterclearMessageBody FilterclearMessage)
parseMessageBody expectedLength MerkleblockCommand =
  parseRemaining expectedLength >> return
    (MerkleblockMessageBody MerkleblockMessage)
parseMessageBody expectedLength SendheadersCommand =
  parseRemaining expectedLength >> return
    (SendheadersMessageBody SendheadersMessage)
parseMessageBody expectedLength FeefilterCommand =
  parseRemaining expectedLength >> return
    (FeefilterMessageBody FeefilterMessage)
parseMessageBody expectedLength SendcmpctCommand =
  parseRemaining expectedLength >> return
    (SendcmpctMessageBody SendcmpctMessage)
parseMessageBody expectedLength CmpctblockCommand =
  parseRemaining expectedLength >> return
    (CpmctblockMessageBody CmpctblockMessage)
parseMessageBody expectedLength GetblocktxnCommand =
  parseRemaining expectedLength >> return
    (GetblocktxnMessageBody GetblocktxnMessage)
parseMessageBody expectedLength BlocktxnCommand =
  parseRemaining expectedLength >> return
    (BlocktxnMessageBody BlocktxnMessage)
parseMessageBody expectedLength UnknownCommand =
  parseRemaining expectedLength >> return
    (UnknownMessageBody UnknownMessage)
