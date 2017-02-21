{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocol.Messages where

import General.Util (checkSum, readFromTable)
import General.Types (HasNetwork(..), getNetwork', getNetwork, Network(..))
import Protocol.MessageBodies


import qualified Data.ByteString as BS
import Data.Binary.Put (Put, putWord32le, putByteString, runPut)
import Data.Binary (Binary(..))
import qualified Data.ByteString.Lazy as BL
import Control.Lens ((^.), makeLenses, over, mapped, _2)
import Data.Binary.Get (Get, getByteString, getWord32le, getWord8, isolate, bytesRead)
import Data.ByteString.Base16 (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Maybe (fromJust, fromMaybe)
import Data.Char (toUpper)

data MessageBody
  = VersionMessageBody VersionMessage
  | VerackMessageBody VerackMessage
  | AddrMessageBody AddrMessage
  | TxMessageBody TxMessage
  | RejectMessageBody RejectMessage
  | PingMessageBody PingMessage
  | PongMessageBody PongMessage
  | InvMessageBody InvMessage
  | GetDataMessageBody GetDataMessage
  | NotFoundMessageBody NotFoundMessage
  | GetBlocksMessageBody GetBlocksMessage
  | GetHeadersMessageBody GetHeadersMessage
  | BlockMessageBody BlockMessage
  | HeadersMessageBody HeadersMessage
  | GetAddrMessageBody GetAddrMessage
  | MempoolMessageBody MempoolMessage
  | CheckorderMessageBody CheckorderMessage
  | SubmitorderMessageBody SubmitorderMessage
  | FilterloadMessageBody  FilterloadMessage
  | FilteraddMessageBody FilteraddMessage
  | FilterclearMessageBody FilterclearMessage
  | MerkleblockMessageBody MerkleblockMessage
  | SendheadersMessageBody SendheadersMessage
  | FeefilterMessageBody FeefilterMessage
  | SendcmpctMessageBody SendcmpctMessage
  | CpmctblockMessageBody CmpctblockMessage
  | GetblocktxnMessageBody GetblocktxnMessage
  | BlocktxnMessageBody BlocktxnMessage
  | UnknownMessageBody UnknownMessage
  deriving (Show, Eq)


data Message = Message MessageBody MessageContext
  deriving (Show, Eq)

data MessageContext = MessageContext
  { _messageContextNetwork :: Network
  } deriving (Show, Eq)

data Header = Header Network Command ByteString
  deriving (Show, Eq)

data Command
  = VersionCommand
  | VerackCommand
  | AddrCommand
  | TxCommand
  | RejectCommand
  | PingCommand
  | PongCommand
  | InvCommand
  | GetDataCommand
  | NotFoundCommand
  | GetBlocksCommand
  | GetHeadersCommand
  | BlockCommand
  | HeadersCommand
  | GetAddrCommand
  | MempoolCommand
  | CheckorderCommand
  | SubmitorderCommand
  | FilterloadCommand
  | FilteraddCommand
  | FilterclearCommand
  | MerkleblockCommand
  | SendheadersCommand
  | FeefilterCommand
  | SendcmpctCommand
  | CmpctblockCommand
  | GetblocktxnCommand
  | BlocktxnCommand
  | UnknownCommand -- If we are unable to find the incoming command
  deriving (Show, Eq)

makeLenses ''MessageContext

instance HasNetwork MessageContext where
  network = messageContextNetwork

instance Binary Message where
  put = putMessage
  get = parseMessage

parseMessage :: Get Message
parseMessage = do
  network <- getNetwork
  command  <- readCommand . encode <$> getByteString 12
  messageLength <- fromIntegral <$> getWord32le
  checksum <- getWord32le
  messageBody <- isolate messageLength (parseMessageBody messageLength command)
  return $ Message messageBody (MessageContext network)

putMessage :: Message -> Put
putMessage (Message messageBody context) = do
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
putMessageBody (VersionMessageBody message) = put message
putMessageBody (GetHeadersMessageBody message) = put message
putMessageBody (GetBlocksMessageBody message) = put message
putMessageBody (HeadersMessageBody message) = put message
putMessageBody (FilterloadMessageBody message) = put message
putMessageBody (InvMessageBody message) = put message
putMessageBody (GetDataMessageBody message) = put message
putMessageBody (RejectMessageBody message) = put message
putMessageBody (MerkleblockMessageBody message) = put message
putMessageBody (TxMessageBody message) = put message
putMessageBody _ = putByteString ""

getCommand :: MessageBody -> Command
getCommand (VersionMessageBody _) = VersionCommand
getCommand (VerackMessageBody _) = VerackCommand
getCommand (AddrMessageBody _) = AddrCommand
getCommand (TxMessageBody _) = TxCommand
getCommand (RejectMessageBody _) = RejectCommand
getCommand (PingMessageBody _) = PingCommand
getCommand (PongMessageBody _) = PongCommand
getCommand (InvMessageBody _) = InvCommand
getCommand (GetDataMessageBody _) = GetDataCommand
getCommand (NotFoundMessageBody _) = NotFoundCommand
getCommand (GetBlocksMessageBody _) = GetBlocksCommand
getCommand (GetHeadersMessageBody _) = GetHeadersCommand
getCommand (BlockMessageBody _) = BlockCommand
getCommand (HeadersMessageBody _) = HeadersCommand
getCommand (GetAddrMessageBody _) = GetAddrCommand
getCommand (MempoolMessageBody _) = MempoolCommand
getCommand (CheckorderMessageBody _) = CheckorderCommand
getCommand (SubmitorderMessageBody _) = SubmitorderCommand
getCommand (FilterloadMessageBody _) = FilterloadCommand
getCommand (FilteraddMessageBody _) = FilteraddCommand
getCommand (FilterclearMessageBody _) = FilterclearCommand
getCommand (MerkleblockMessageBody _) = MerkleblockCommand
getCommand (SendheadersMessageBody _) = SendheadersCommand
getCommand (FeefilterMessageBody _) = FeefilterCommand
getCommand (SendcmpctMessageBody _) = SendcmpctCommand
getCommand (CpmctblockMessageBody _) = CmpctblockCommand
getCommand (GetblocktxnMessageBody _) = GetblocktxnCommand
getCommand (BlocktxnMessageBody _) = BlocktxnCommand
getCommand (UnknownMessageBody _) = UnknownCommand

commandTable :: [(Command, ByteString)]
commandTable = over (mapped . _2) getCommandBS commandTable'

commandTableBinary :: [(Command, ByteString)]
commandTableBinary = over (mapped . _2) Char8.pack commandTable'

getCommand' :: Command -> ByteString
getCommand' = padWithZeroes . searchCommand
  where 
  searchCommand = fromJust . flip lookup commandTableBinary
  padWithZeroes bs = bs `BS.append` padding bs
  padding bs = BS.replicate (12 - BS.length bs) 0
  -- need to pad with zeroes

commandTable' :: [(Command, String)]
commandTable' = 
  [ (VersionCommand, "version") 
  , (VerackCommand , "verack")
  , (AddrCommand,    "addr")
  , (TxCommand,      "tx")
  , (RejectCommand,  "reject")
  , (PingCommand, "ping")
  , (PongCommand, "pong")
  , (InvCommand, "inv")
  , (GetDataCommand, "getdata")
  , (NotFoundCommand, "notfound")
  , (GetBlocksCommand, "getblocks")
  , (GetHeadersCommand, "getheaders")
  , (BlockCommand, "block")
  , (HeadersCommand, "headers")
  , (GetAddrCommand, "getaddr")
  , (MempoolCommand, "mempool")
  , (CheckorderCommand, "checkorder")
  , (SubmitorderCommand, "submitorder")
  , (FilterloadCommand, "filterload")
  , (FilteraddCommand, "filteradd")
  , (FilterclearCommand, "filterclear")
  , (MerkleblockCommand, "merkleblock")
  , (SendheadersCommand, "sendheaders")
  , (FeefilterCommand, "feefilter")
  , (SendcmpctCommand, "send")
  , (CmpctblockCommand, "cmpctblock")
  , (GetblocktxnCommand, "getblocktxt")
  , (BlocktxnCommand, "blocktxn")]

getCommandBS :: String -> ByteString
getCommandBS = Char8.pack . padWithZeroes . map toUpper . Char8.unpack .  encode . Char8.pack
  where padWithZeroes str = str ++ padding str
        padding str = replicate (24 - length str) '0'        

printCommand :: Command -> ByteString
printCommand = fromJust . flip lookup commandTable

readCommand :: ByteString -> Command
readCommand bs = fromMaybe UnknownCommand (readFromTable commandTable bs)

getPayload :: Get ByteString
getPayload = do
  payloadLength <- fromIntegral <$> getWord8
  getByteString payloadLength

parseRemaining :: Int -> Get ()
parseRemaining expectedLength = do
  bytesRead' <- fromIntegral <$> bytesRead 
  getByteString (expectedLength - bytesRead')
  return ()

parseMessageBody :: Int -> Command -> Get MessageBody
parseMessageBody _ VersionCommand =
  VersionMessageBody <$> get
-- These should all at least consume their length!
parseMessageBody expectedLength VerackCommand =
  parseRemaining expectedLength >> return
    (VerackMessageBody VerackMessage)
parseMessageBody expectedLength AddrCommand =
  parseRemaining expectedLength >> return
    (AddrMessageBody AddrMessage)
parseMessageBody _ TxCommand =
  TxMessageBody <$> get
parseMessageBody expectedLength RejectCommand = do
  message <- RejectMessageBody <$> get
  parseRemaining expectedLength
  -- TODO: The reject message may or may not have a data field
  --       That is why parseRemaining is used here
  return message
parseMessageBody expectedLength PingCommand =
  parseRemaining expectedLength >> return
    (PingMessageBody PingMessage)
parseMessageBody expectedLength PongCommand =
  parseRemaining expectedLength >> return
    (PongMessageBody PongMessage)
parseMessageBody _ InvCommand = InvMessageBody <$> get  
parseMessageBody _ GetDataCommand = GetDataMessageBody <$> get
parseMessageBody _ GetHeadersCommand =  GetHeadersMessageBody <$> get
parseMessageBody _ GetBlocksCommand = GetBlocksMessageBody <$> get
parseMessageBody expectedLength BlockCommand =
  parseRemaining expectedLength >> return
    (BlockMessageBody BlockMessage)
parseMessageBody _ HeadersCommand = HeadersMessageBody <$> get
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
parseMessageBody _ FilterloadCommand = FilterloadMessageBody <$> get  
parseMessageBody expectedLength FilteraddCommand =
  parseRemaining expectedLength >> return
    (FilteraddMessageBody FilteraddMessage)
parseMessageBody expectedLength FilterclearCommand =
  parseRemaining expectedLength >> return
    (FilterclearMessageBody FilterclearMessage)
parseMessageBody _ MerkleblockCommand = MerkleblockMessageBody <$> get
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
