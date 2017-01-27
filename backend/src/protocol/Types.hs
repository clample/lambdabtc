{-# LANGUAGE OverloadedStrings #-}

module Protocol.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Lens (over, _2, mapped)
import Data.Maybe (fromJust)
import Data.Char (toUpper)
import Data.Tuple (swap)
import Data.ByteString.Base16 (decode, encode)

data Addr = Addr IP Port
  deriving (Show, Eq)

type IP = (Int, Int, Int, Int)

type Port = Int

data Message = Message MessageBody MessageContext
  deriving (Show, Eq)

data MessageBody
  = VersionMessage
    { version    :: Int
    , nonceInt   :: Integer -- Nonce can be 8 bytes -> use integer 
    , lastBlockN :: Integer
    , senderAddr :: Addr
    , peerAddr   :: Addr
    , relay      :: Bool
    }
  | VerackMessage
  | AddrMessage
  | TxMessage
  | RejectMessage
  | PingMessage
  | PongMessage
  | InvMessage
  | GetDataMessage
  | NotFoundMessage
  | GetBlocksMessage
  | GetHeadersMessage
  | BlockMessage
  | HeadersMessage
  | GetAddrMessage
  | MempoolMessage
  | CheckorderMessage
  | SubmitorderMessage
  | FilterloadMessage
  | FilteraddMessage
  | FilterclearMessage
  | MerkleblockMessage
  | SendheadersMessage
  | FeefilterMessage
  | SendcmpctMessage
  | CmpctblockMessage
  | GetblocktxnMessage
  | BlocktxnMessage
  deriving (Show, Eq)

getCommand :: MessageBody -> Command
getCommand (VersionMessage {}) = VersionCommand
getCommand VerackMessage = VerackCommand
getCommand AddrMessage = AddrCommand
getCommand TxMessage = TxCommand
getCommand RejectMessage = RejectCommand
getCommand PingMessage = PingCommand
getCommand PongMessage = PongCommand
getCommand InvMessage = InvCommand
getCommand GetDataMessage = GetDataCommand
getCommand NotFoundMessage = NotFoundCommand
getCommand GetBlocksMessage = GetBlocksCommand
getCommand BlockMessage = BlockCommand
getCommand HeadersMessage = HeadersCommand
getCommand GetAddrMessage = GetAddrCommand
getCommand MempoolMessage = MempoolCommand
getCommand CheckorderMessage = CheckorderCommand
getCommand SubmitorderMessage = SubmitorderCommand
getCommand FilterloadMessage = FilterloadCommand
getCommand FilteraddMessage = FilteraddCommand
getCommand FilterclearMessage = FilterclearCommand
getCommand MerkleblockMessage = MerkleblockCommand
getCommand SendheadersMessage = SendheadersCommand
getCommand FeefilterMessage = FeefilterCommand
getCommand SendcmpctMessage = SendcmpctCommand
getCommand CmpctblockMessage = CmpctblockCommand
getCommand GetblocktxnMessage = GetblocktxnCommand
getCommand BlocktxnMessage = BlocktxnCommand

data MessageContext = MessageContext
  { network :: Network
  , time    :: POSIXTime 
  } deriving (Show, Eq)

data Network = TestNet3 | MainNet
  deriving (Show, Eq)

networkTable :: [(Network, ByteString)]
networkTable =
  [ (TestNet3, "0B110907")
  , (MainNet,  "F9BEB4D9")]

printNetwork :: Network -> ByteString
printNetwork = fromJust . flip lookup networkTable

readNetwork :: ByteString -> Maybe Network
readNetwork = readFromTable networkTable

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
  deriving (Show, Eq)

commandTable :: [(Command, ByteString)]
commandTable = over (mapped . _2) (getCommandBS)                               
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
getCommandBS = Char8.pack . (padWithZeroes) . map toUpper . Char8.unpack .  encode . Char8.pack
  where padWithZeroes str = str ++ (padding str)
        padding str = replicate (24 - length str) '0'        

printCommand :: Command -> ByteString
printCommand = fromJust . flip lookup commandTable

readCommand :: ByteString -> Maybe Command
readCommand = readFromTable commandTable

readFromTable :: [(a, ByteString)] -> ByteString -> Maybe a
readFromTable table = lookupInTable . uppercase
  where
    uppercase     = Char8.pack . (map toUpper) . Char8.unpack
    lookupInTable = flip lookup (map swap table)
