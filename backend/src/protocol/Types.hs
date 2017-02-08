{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: Split this module up. Do explicit exports

module Protocol.Types where

import BitcoinCore.BlockHeaders (BlockHash(..), BlockHeader(..))
import BitcoinCore.BloomFilter (Filter(..), Tweak(..), NFlags(..))
import BitcoinCore.Inventory (InventoryVector(..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Lens (over, _2, mapped)
import Data.Maybe (fromJust, fromMaybe)
import Data.Char (toUpper)
import Data.Tuple (swap)
import Data.ByteString.Base16 (decode, encode)
import Data.Binary.Get (Get, getByteString)
import Control.Lens (makeFields)

data Addr = Addr IP Port
  deriving (Show, Eq)

type IP = (Int, Int, Int, Int)

type Port = Int

data Network = TestNet3 | MainNet
  deriving (Show, Eq)

data MessageContext = MessageContext
  { _messageContextNetwork :: Network
  } deriving (Show, Eq)

makeFields ''MessageContext


data VersionMessage = VersionMessage
    { _versionMessageVersion    :: Int
    , _versionMessageNonce   :: Integer -- Nonce can be 8 bytes -> use integer 
    , _versionMessageLastBlock :: Integer
    , _versionMessageSenderAddr :: Addr
    , _versionMessagePeerAddr   :: Addr
    , _versionMessageRelay      :: Bool
    , _versionMessageTime       :: POSIXTime
    } deriving (Show, Eq)
makeFields ''VersionMessage
    
data VerackMessage = VerackMessage
  deriving (Show, Eq)

data AddrMessage = AddrMessage
  deriving (Show, Eq)

data TxMessage = TxMessage
  deriving (Show, Eq)
  

data RejectMessage = RejectMessage
  deriving (Show, Eq)

data PingMessage = PingMessage
  deriving (Show, Eq)

data PongMessage = PongMessage
  deriving (Show, Eq)

data InvMessage = InvMessage
  { _invMessageInvVectors :: [InventoryVector]}
  deriving (Show, Eq)

data GetDataMessage = GetDataMessage
  deriving (Show, Eq)

data NotFoundMessage = NotFoundMessage
  deriving (Show, Eq)

data GetBlocksMessage = GetBlocksMessage
  deriving (Show, Eq)

data GetHeadersMessage = GetHeadersMessage
    { _getHeadersMessageVersion            :: Int
    , _getHeadersMessageBlockLocatorHashes :: [BlockHash]
    , _getHeadersMessageHashStop           :: BlockHash}
  deriving (Show, Eq)

data BlockMessage = BlockMessage
  deriving (Show, Eq)

data HeadersMessage = HeadersMessage
  { _headersMessageBlockHeaders :: [BlockHeader]}
  deriving (Show, Eq)

data GetAddrMessage = GetAddrMessage
  deriving (Show, Eq)

data MempoolMessage = MempoolMessage
  deriving (Show, Eq)

data CheckorderMessage = CheckorderMessage
  deriving (Show, Eq)

data SubmitorderMessage = SubmitorderMessage
  deriving (Show, Eq)

data FilterloadMessage = FilterloadMessage
    { _filterLoadMessageFilter :: Filter
    , _filterLoadMessagenHashFuncs :: Int
    , _filterLoadMessageNTweak :: Tweak
    , _filterLoadMessageNFlags :: NFlags
    }
  deriving (Show, Eq)

data FilteraddMessage = FilteraddMessage
  deriving (Show, Eq)

data FilterclearMessage = FilterclearMessage
  deriving (Show, Eq)

data MerkleblockMessage = MerkleblockMessage
  deriving (Show, Eq)

data SendheadersMessage = SendheadersMessage
  deriving (Show, Eq)

data FeefilterMessage = FeefilterMessage
  deriving (Show, Eq)

data SendcmpctMessage = SendcmpctMessage
  deriving (Show, Eq)

data GetblocktxnMessage = GetblocktxnMessage
  deriving (Show, Eq)

data BlocktxnMessage = BlocktxnMessage
  deriving (Show, Eq)

data UnknownMessage = UnknownMessage
  deriving (Show, Eq)

data CmpctblockMessage = CmpctblockMessage
  deriving (Show, Eq)

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

networkTable :: [(Network, ByteString)]
networkTable =
  [ (TestNet3, "0B110907")
  , (MainNet,  "F9BEB4D9")]

printNetwork :: Network -> ByteString
printNetwork = fromJust . flip lookup networkTable

getNetwork' :: Network -> ByteString
getNetwork' = fst . decode . printNetwork

readNetwork :: ByteString -> Maybe Network
readNetwork = readFromTable networkTable

getNetwork :: Get Network
getNetwork = do
  mNetwork <- readNetwork . encode <$> getByteString 4
  case mNetwork of
    Just network' -> return network'
    Nothing -> fail "Unable to parse netowrk"

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

readFromTable :: [(a, ByteString)] -> ByteString -> Maybe a
readFromTable table = lookupInTable . uppercase
  where
    uppercase     = Char8.pack . map toUpper . Char8.unpack
    lookupInTable = flip lookup (map swap table)
