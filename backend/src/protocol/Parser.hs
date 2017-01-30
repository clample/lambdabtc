{-# LANGUAGE OverloadedStrings #-}

module Protocol.Parser where

import Text.Megaparsec (Parsec, Dec, hexDigitChar, count, many, manyTill, eof, getPosition, setPosition, lookAhead, parseMaybe, (<|>), try, parse)
import Messages (showMessageBody)
import Protocol.Types (MessageBody(..), Header(..), readNetwork, readCommand, Network(..), Command(..), Addr(..), Message(..))
import qualified Data.ByteString.Char8 as Char8
import Numeric (readHex)
import Util (switchEndian, parsePayload, parseBool)
import qualified Debug.Trace as Trace
import Data.Binary.Get (Get(..), getByteString, getWord32le, getWord64be, getWord64le, getWord8, getWord16be, isolate, bytesRead)
import Data.ByteString.Base16 (encode, decode)
import Foreign.Marshal.Utils (toBool)
import Data.ByteString (ByteString)

parseMessage :: Get MessageBody
parseMessage = do
  mNetwork <- readNetwork . encode <$> getByteString 4
  command  <- readCommand . encode <$> getByteString 12
  messageLength <- fromIntegral <$> getWord32le
  checksum <- getWord32le
  isolate messageLength (parseMessageBody messageLength command)
  
  
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

parseMessageBody expectedLength VersionCommand = do
  version     <- fromIntegral <$> getWord32le
  services    <- getWord64be
  timestamp   <- fromIntegral <$> getWord64le
  peer        <- getAddr
  sender      <- getAddr
  nonce       <- fromIntegral <$> getWord64be
  userAgent   <- getPayload
  startHeight <- fromIntegral <$> getWord32le
  relay       <- toBool <$> getWord8 
  return $ VersionMessage version nonce startHeight sender peer relay

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
parseMessageBody expectedLength InvCommand =
  parseRemaining expectedLength >> return InvMessage
parseMessageBody expectedLength GetDataCommand =
  parseRemaining expectedLength >> return GetDataMessage
parseMessageBody expectedLength GetHeadersCommand =
  parseRemaining expectedLength >> return GetHeadersMessage
parseMessageBody expectedLength BlockCommand =
  parseRemaining expectedLength >> return BlockMessage
parseMessageBody expectedLength HeadersCommand =
  parseRemaining expectedLength >> return HeadersMessage
parseMessageBody expectedLength GetAddrCommand =
  parseRemaining expectedLength >> return GetAddrMessage
parseMessageBody expectedLength MempoolCommand =
  parseRemaining expectedLength >> return MempoolMessage
parseMessageBody expectedLength CheckorderCommand =
  parseRemaining expectedLength >> return CheckorderMessage
parseMessageBody expectedLength SubmitorderCommand =
  parseRemaining expectedLength >> return SubmitorderMessage
parseMessageBody expectedLength FilterloadCommand =
  parseRemaining expectedLength >> return FilterloadMessage
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
