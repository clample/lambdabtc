{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocol.Messages where

import General.Util (reverseLookup, unroll, Endian(..))
import General.Types (HasNetwork(..), Network(..))
import Protocol.MessageBodies
import BitcoinCore.BloomFilter (Filter(..), FilterContext(..), Tweak(..))
import General.Hash (CheckSum(..), checksum)

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
import Data.Tuple (swap)

import Test.QuickCheck.Arbitrary (Arbitrary(..), vector, arbitraryBoundedEnum)
import Test.QuickCheck.Gen (oneof, Gen, choose, vectorOf)

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
                   -- TODO: Maybe it is misleading to have this extra constructor
                   --       and it would make more sense to use Maybe
  deriving (Show, Eq)

makeLenses ''MessageContext

instance HasNetwork MessageContext where
  network = messageContextNetwork

instance Binary Message where
  put = putMessage
  get = parseMessage

parseMessage :: Get Message
parseMessage = do
  network' <- get
  command'  <- get
  messageLength' <- fromIntegral <$> getWord32le
  checksum' <- getWord32le
  -- TODO: Checksum for the message should maybe be verified
  messageBody' <- isolate messageLength' (parseMessageBody messageLength' command')
  return $ Message messageBody' (MessageContext network')

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
putHeader (Header network' command' message') = do
  put network'
  put command'
  putWord32le $ fromIntegral (BS.length message')
  put $ checksum message'


putMessageBody :: MessageBody -> Put
putMessageBody (VersionMessageBody message) = put message
putMessageBody (GetHeadersMessageBody message) = put message
putMessageBody (GetBlocksMessageBody message) = put message
putMessageBody (HeadersMessageBody message) = put message
putMessageBody (FilterloadMessageBody message) = put message
putMessageBody (FilteraddMessageBody message) = put message
putMessageBody (InvMessageBody message) = put message
putMessageBody (GetDataMessageBody message) = put message
putMessageBody (RejectMessageBody message) = put message
putMessageBody (MerkleblockMessageBody message) = put message
putMessageBody (TxMessageBody message) = put message
putMessageBody (PingMessageBody message) = put message
putMessageBody (PongMessageBody message) = put message
putMessageBody _ = putByteString ""

-- TODO: We're typically using `getX`
--       as the name for deserializing X
--       it may make sense to rename this function
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

instance Binary Command where
  put = putCommand
  get = deserializeCommand

putCommand :: Command -> Put
putCommand command =
  case lookup command commandTable of
    Just bs -> putByteString bs
    Nothing -> error $ "Unable to serialize command " ++ show command

deserializeCommand :: Get Command
deserializeCommand = do
  bs <- getByteString 12
  return $
    fromMaybe UnknownCommand (reverseLookup bs commandTable)
  -- TODO: UnknownCommand is not an actual bitcoin command
  --       We should return `Maybe Command` instead

getCommandBS :: String -> ByteString
getCommandBS = padWithZeroes . Char8.pack
  where padWithZeroes bs = bs `BS.append` padding bs
        padding bs = BS.replicate (12 - BS.length bs) 0 

commandTable :: [(Command, ByteString)]
commandTable = over (mapped . _2) getCommandBS commandTable'

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
parseMessageBody _ PingCommand =
  PingMessageBody <$> get
parseMessageBody _ PongCommand =
  PongMessageBody <$> get
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
parseMessageBody _ FilteraddCommand = FilteraddMessageBody <$> get
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
parseMessageBody expectedLength NotFoundCommand =
  parseRemaining expectedLength >> return
    (NotFoundMessageBody NotFoundMessage)
parseMessageBody expectedLength UnknownCommand =
  parseRemaining expectedLength >> return
    (UnknownMessageBody UnknownMessage)

instance Arbitrary Message where
  arbitrary = Message <$> arbitrary <*> arbitrary

instance Arbitrary MessageBody where
  arbitrary = oneof
    [ arbitraryVersionMessage
    , arbitraryGetHeadersMessage
    , arbitraryGetHeadersMessage
    , arbitraryMerkleblockMessage
    , arbitraryFilterloadMessage
    , arbitraryFilteraddMessage
    , arbitraryInvMessage
    , arbitraryGetDataMessage
    , arbitraryRejectMessage
    , arbitraryPingMessage
    , return (VerackMessageBody VerackMessage)]

instance Arbitrary MessageContext where
  arbitrary = MessageContext <$> arbitrary

arbitraryVersionMessage = do
  version    <- choose (0, maxVersion)
  nonce'     <- arbitrary
  lastBlock' <- arbitrary
  senderAddr <- arbitrary
  peerAddr   <- arbitrary
  relay      <- arbitrary
  time       <- choose (0, maxTime) :: Gen Integer
  return $ VersionMessageBody
    (VersionMessage version nonce' lastBlock' senderAddr peerAddr relay (realToFrac time))
  where
    maxVersion = 0xffffffff         -- 4 bytes
    maxNonce   = 0xffffffffffffffff -- 8 bytes
    maxTime = 0xffffffffffffffff -- 8 bytes

arbitraryGetHeadersMessage = do
  version <- choose (0, maxVersion)
  n      <- choose (0, 2000)
  blockLocatorHashes <- vectorOf n arbitrary
  hashStop <- arbitrary
  return $ GetHeadersMessageBody
    (GetHeadersMessage version blockLocatorHashes hashStop)
  where maxVersion = 0xffffffff -- 4 bytes

arbitraryMerkleblockMessage = do
  blockHeader' <- arbitrary
  nMerkleHashes <- choose (0, 100)
  merkleHashes' <- vectorOf  nMerkleHashes arbitrary
  flags' <- arbitrary
  return $ MerkleblockMessageBody
    (MerkleblockMessage blockHeader' merkleHashes' flags')

arbitraryHeadersMessage = do
  n            <- choose (0, 2000) -- A headers message contains at most 2000 block headers
  blockHeaders <- vectorOf n arbitrary
  return $ HeadersMessageBody
    (HeadersMessage blockHeaders)

arbitraryFilterloadMessage = do
  fValue <- choose (0, 0xffffffffffffffff) -- upper limit is so the value is reasonably sized
  let minEncodingLength = BS.length . unroll LE $ fValue
  fLengthBytes <- choose (minEncodingLength, 2 * minEncodingLength)
  let filter = Filter { _filterLengthBytes = fLengthBytes, _filterValue = fValue}
  nHashFuncs <- choose (0, maxNHashFuncs)
  nTweak <- Tweak <$> choose (0, maxNTweak)
  nFlags <- arbitraryBoundedEnum
  let filterContext' = FilterContext { _tweak = nTweak, _nHashFunctions = nHashFuncs}
  return $ FilterloadMessageBody
    (FilterloadMessage  filter filterContext' nFlags)
  where
    maxNHashFuncs = 0xffffffff -- 4 bytes
    maxNTweak     = 0xffffffff -- 4 bytes

arbitraryPingMessage = do
  PingMessageBody . PingMessage <$> arbitrary

arbitraryFilteraddMessage = do
  filterdataLength <- choose (1, 520)
  filterdata <- Char8.pack <$> vector filterdataLength
  return $ FilteraddMessageBody $ FilteraddMessage filterdata

arbitraryInvMessage = 
  InvMessageBody . InvMessage <$> arbitrary

arbitraryGetDataMessage =
  GetDataMessageBody . GetDataMessage <$> arbitrary

arbitraryRejectMessage = do
  RejectMessageBody <$>
    (RejectMessage <$> arbitrary <*> arbitrary <*> arbitrary)
