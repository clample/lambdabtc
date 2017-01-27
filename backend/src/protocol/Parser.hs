{-# LANGUAGE OverloadedStrings #-}

module Protocol.Parser where

import Text.Megaparsec (Parsec, Dec, hexDigitChar, count, many, manyTill, eof, getPosition, setPosition, lookAhead, parseMaybe, (<|>), try, parse)
import Messages (showMessageBody)
import Protocol.Types (MessageBody(..), Header(..), readNetwork, readCommand, Network(..), Command(..), Addr(..), Message(..))
import qualified Data.ByteString.Char8 as Char8
import Numeric (readHex)
import Util (switchEndian, parsePayload, parseBool)
import qualified Debug.Trace as Trace


parseMessage :: String -> Maybe MessageBody
parseMessage input = do
  header@(Header network command messageBody) <- parseMaybe parseHeader input
  parseMaybe (parseMessageBody command) (Char8.unpack messageBody)

-- TODO: 
parseMessage' :: String -> [Maybe MessageBody]
parseMessage' input =
  case eitherHeaders of
    Left error     -> Trace.trace (show error) []
    Right headers -> map parseBody headers
  where
    eitherHeaders = parse (many parseHeader) "" input -- WARNING: This silently lets some messages fail to parse
    parseBody (Header network command messageBody) = parseMaybe (parseMessageBody command) (Char8.unpack messageBody)

parseHeader :: Parsec Dec String Header
parseHeader = do
  mNetwork       <- readNetwork . Char8.pack <$> count 8 hexDigitChar
  mCommand       <- readCommand . Char8.pack <$> count 24 hexDigitChar
  messageLength  <- (fst . head . readHex . Char8.unpack . switchEndian . Char8.pack) <$> count 8 hexDigitChar
  checksum       <- count 8 hexDigitChar
  message        <- Char8.pack <$> count (messageLength * 2) hexDigitChar
  case (mNetwork, mCommand) of
    (Just network, Just command) -> return $ Header network command message
    (_, _)                       -> fail $ "Unable to parse header: " ++ show mNetwork ++ "----" ++ show mCommand 

parseMessageBody :: Command -> Parsec Dec String MessageBody

parseMessageBody VersionCommand = do
  version              <- (fst . head . readHex . Char8.unpack . switchEndian . Char8.pack) <$> count 8 hexDigitChar
  services             <- count 16 hexDigitChar
  timestamp            <- parseTimestamp
  peer                 <- parseAddr
  sender               <- parseAddr
  nonce                <- (fst . head . readHex) <$> count 16 hexDigitChar
  userAgent            <- Char8.unpack <$> parsePayload
  startHeight          <- (fst . head . readHex . Char8.unpack . switchEndian . Char8.pack) <$> count 8 hexDigitChar
  relay                <- try parseBool <|> return False
  eof
  return $ VersionMessage version nonce startHeight sender peer relay
  where
    parseTimestamp = (fst . head . readHex . Char8.unpack . switchEndian . Char8.pack) <$> count 16 hexDigitChar

parseMessageBody VerackCommand = return VerackMessage
parseMessageBody AddrCommand = return AddrMessage
parseMessageBody TxCommand = return TxMessage
parseMessageBody RejectCommand = return RejectMessage
parseMessageBody PingCommand = return PingMessage
parseMessageBody PongCommand = return PongMessage
parseMessageBody InvCommand = return InvMessage
parseMessageBody GetDataCommand = return GetDataMessage
parseMessageBody GetHeadersCommand = return GetHeadersMessage
parseMessageBody BlockCommand = return BlockMessage
parseMessageBody HeadersCommand = return HeadersMessage
parseMessageBody GetAddrCommand = return GetAddrMessage
parseMessageBody MempoolCommand = return MempoolMessage
parseMessageBody CheckorderCommand = return CheckorderMessage
parseMessageBody SubmitorderCommand = return SubmitorderMessage
parseMessageBody FilterloadCommand = return FilterloadMessage
parseMessageBody FilteraddCommand = return FilteraddMessage
parseMessageBody FilterclearCommand = return FilterclearMessage
parseMessageBody MerkleblockCommand = return MerkleblockMessage
parseMessageBody SendheadersCommand = return SendheadersMessage
parseMessageBody FeefilterCommand = return FeefilterMessage
parseMessageBody SendcmpctCommand = return SendcmpctMessage
parseMessageBody CmpctblockCommand = return CmpctblockMessage
parseMessageBody GetblocktxnCommand = return GetblocktxnMessage
parseMessageBody BlocktxnCommand = return BlocktxnMessage

parseAddr :: Parsec Dec String Addr
parseAddr = do
  count 16  hexDigitChar
  count 24 hexDigitChar -- parse ipv6 magic str
  a <- parseIpComponent
  b <- parseIpComponent
  c <- parseIpComponent
  d <- parseIpComponent
  port <- parsePort
  return $ Addr (a, b, c, d) port
  where
    parseIpComponent = (fst . head . readHex) <$> count 2 hexDigitChar
    parsePort = (fst . head . readHex) <$> count 4 hexDigitChar
