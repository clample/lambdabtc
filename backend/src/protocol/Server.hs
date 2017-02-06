{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol.Server where

import Protocol.Parser (parseMessage)
import Protocol.Messages (getAddr, putMessage)
import Protocol.Types (Network(..), Addr(..), MessageContext(..), Message(..), MessageBody(..), genesisHash)
import Network.Socket (Socket)
import Protocol.Network (Peer(..), connectToPeer)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Monad.State.Lazy (StateT(..), runStateT, liftIO, get, gets)
import Control.Monad.Reader (runReaderT, ask)
import qualified Control.Monad.State.Lazy as State
import System.Random (randomR, StdGen, getStdGen)
import Conduit (Producer(..), runConduit, (.|), mapC, mapMC)
import Data.Conduit.Combinators (encodeBase16, stdout)
import Data.Conduit.Network (sourceSocket, sinkSocket)
import Data.Conduit.Serialization.Binary (conduitGet, conduitPut)
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan, newTBMChan, TBMChan, writeTBMChan, readTBMChan)
import Control.Concurrent.STM (atomically, STM(..))
import Control.Concurrent (forkIO)
import Data.Binary.Put (runPut)
import Data.Binary (Binary(..))
import Data.ByteString.Base16 (decode)
import BlockHeaders (BlockHash(..), encodeBlockHeader)
import Server.Config (ConfigM(..), Config(..), developmentConfig)
import Database.Persist.Sql (insertMany_, count, runSqlPool, Filter, toSqlKey, insert_)
import qualified Database.Persist.Sql as DB
import Persistence (runDB, PersistentBlockHeader(..))
import Data.List.Split (chunksOf)
import BlockHeaders (BlockHeader(..), decodeBlockHeader, hashBlock, genesisBlockTestnet, verifyHeaders)
import Data.Maybe (fromJust)
import BloomFilter (pDefault, blankFilter, updateFilter, numberHashFunctions, filterSize, hardcodedTweak, NFlags(..))

data ConnectionContext = ConnectionContext
  { version' :: Int
  , lastBlock' :: Integer
  , myAddr' :: Addr
  , peer' :: Peer
  , relay' :: Bool
    -- https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#extensions-to-existing-messages
    -- Relay should be set to False when functioning as an SPV node
  , network' :: Network
  , writerChan :: TBMChan Message
  , listenChan :: TBMChan Message
  , time :: POSIXTime
  , randGen :: StdGen
  } 

type Connection a = StateT ConnectionContext ConfigM a

runConnection :: Connection a -> ConnectionContext -> Config -> IO (a, ConnectionContext)
runConnection connection state config =
  runReaderT (runConfigM (runStateT connection state)) config

connectTestnet :: Config -> IO () 
connectTestnet config = do
  persistGenesisBlock config
  context <- getConnectionContext config
  let listenChan' = listenChan context
      writerChan' = writerChan context
      Peer peerSocket _ = peer' context
  forkIO $ listener listenChan' peerSocket
  forkIO $ writer writerChan' peerSocket
  runConnection connection context config
  return ()

getConnectionContext :: Config -> IO ConnectionContext
getConnectionContext config = do
  peer <- connectToPeer 1
  writeChan <- atomically $ newTBMChan 16
  listenChan <- atomically $ newTBMChan 16
  time' <- getPOSIXTime
  randGen' <- getStdGen
  lastBlock <- fromIntegral <$> getLastBlock config
  return ConnectionContext
        { version' = 60002
        , lastBlock' = lastBlock
        , myAddr' = Addr (0, 0, 0, 0) 18333 
        , peer' = peer
        , relay' = False 
        , network' = TestNet3
        , writerChan = writeChan
        , listenChan = listenChan
        , time = time'
        , randGen = randGen'
        }

getLastBlock :: Config -> IO Int
getLastBlock Config {pool = pool} =
  ((+) (-1)) <$> runSqlPool lastBlockQuery pool
  where lastBlockQuery = count allBlocksFilter
        allBlocksFilter = [] :: [Filter PersistentBlockHeader]

persistGenesisBlock :: Config -> IO ()
persistGenesisBlock config@(Config {pool = pool}) = do
  lastBlock <- getLastBlock config
  if (lastBlock == -1)
    then runSqlPool (insert_ $ encodeBlockHeader genesisBlockTestnet) pool
    else return ()

instance Binary Message where
  put = putMessage
  get = parseMessage


listener :: TBMChan Message -> Socket -> IO ()
listener chan socket = runConduit
  $  sourceSocket socket
  .| conduitGet parseMessage
  .| logMessages "incoming"
  .| sinkTBMChan chan True 


writer :: TBMChan Message  -> Socket -> IO ()
writer chan socket = runConduit
  $  sourceTBMChan chan
  .| logMessages "outgoing"
  .| mapC put
  .| conduitPut
  .| sinkSocket socket


logMessages context =
  mapMC logAndReturn
  where logAndReturn message = do
          putStrLn $ context ++ " " ++ show message
          return message

connection :: Connection ()
connection = do
  sendVersion
  setFilter
  connectionLoop

connectionLoop :: Connection ()
connectionLoop = do
  chan <- State.gets listenChan
  mResponse <-  liftIO . atomically . readTBMChan $ chan
  case mResponse of
    Nothing -> fail "listenChan is closed and empty"
    Just response -> (handleResponse response)
  connectionLoop

  
handleResponse :: Message -> Connection ()

handleResponse (Message (VersionMessage {lastBlockN = lastBlockPeer}) _) = do
  connectionContext <- State.get
  let
    network = network' connectionContext
    writerChan' = writerChan connectionContext
    lastBlock = lastBlock' connectionContext
    verackMessage = Message VerackMessage (MessageContext network)
  liftIO . atomically $ writeTBMChan writerChan' verackMessage
  if (lastBlockPeer > lastBlock)
    then synchronizeHeaders lastBlockPeer
    else return ()

handleResponse (Message PingMessage _) = do
  connectionContext <- State.get
  let
    network = network' connectionContext
    writerChan' = writerChan connectionContext
    pongMessage = Message PongMessage (MessageContext network)
  liftIO . atomically $ writeTBMChan writerChan' pongMessage

handleResponse (Message (HeadersMessage headers) _) = do
  let persistentHeaders = map encodeBlockHeader headers
      chunkedPersistentHeaders = chunksOf 100 persistentHeaders
      -- Headers are inserted in chunks
      -- sqlite rejects if we insert all at once
  config <- ask
  mostRecentHeader <- getMostRecentHeader
  let isValid = verifyHeaders (mostRecentHeader:headers)
  if isValid
    then do
    runDB $ mapM_ insertMany_ chunkedPersistentHeaders
    State.modify (\context@(ConnectionContext {lastBlock' = lastBlock})
                 -> context {lastBlock' = lastBlock + (fromIntegral .length) headers})
    else fail "We recieved invalid headers"
    
handleResponse _ = do
  return ()

getMostRecentHeader :: Connection BlockHeader
getMostRecentHeader = do
  config <- ask
  blockHeaderCount <- liftIO $ fromIntegral <$> getLastBlock config
  mlastBlockHeader <- runDB $ DB.get (toSqlKey . fromIntegral $ blockHeaderCount + 1)
  -- NOTE: we query by i + 1 since the genesis block (block 0) is in the db at index 1
  case mlastBlockHeader of
    Nothing -> fail "Unable to get most recent block header. This should never happen"
    Just lastBlockHeader -> return (decodeBlockHeader lastBlockHeader)

sendVersion :: Connection ()
sendVersion = do
  connectionContext <- State.get
  let (ConnectionContext
       { version' = version'
       , lastBlock' = lastBlock'
       , relay' = relay'
       , myAddr' = myAddr'
       , peer' = Peer peerSocket peerAddr'
       , network' = network'
       , writerChan = writerChan
       , time = time
       , randGen = randGen
       }) = connectionContext
      nonce' = fst $ randomR (0, 0xffffffffffffffff ) randGen
      versionMessage = Message
          (VersionMessage version' nonce' lastBlock' peerAddr' myAddr' relay' time)
          (MessageContext network')
  liftIO . atomically $ writeTBMChan writerChan versionMessage


setFilter :: Connection ()
setFilter = do
 writerChan <- State.gets writerChan
 network' <- State.gets network'
 let
   blank = blankFilter 1 pDefault
   s = filterSize 1 pDefault
   nHashFuncs = numberHashFunctions s 1
   txId = fst . decode $ "b73619d208b4f7b91cc93185b1e2f5057bacbe9b5c0b63c36159e0354be0a77f"
   filter' = updateFilter nHashFuncs hardcodedTweak txId blank
   filterloadMessage = Message
     (FilterloadMessage filter' nHashFuncs hardcodedTweak BLOOM_UPDATE_NONE)
     (MessageContext network')
 liftIO . atomically $ writeTBMChan writerChan filterloadMessage


synchronizeHeaders :: Integer -> Connection ()
synchronizeHeaders lastBlockPeer = do
  lastBlock <- State.gets lastBlock'
  if (lastBlock < lastBlockPeer)
    then do
      getHeaders
      handleMessages
      synchronizeHeaders lastBlockPeer
    else return ()
  where
    -- Keep reading messages until we get a headers message
    handleMessages = do
      chan <- State.gets listenChan
      mResponse <-  liftIO . atomically . readTBMChan $ chan
      case mResponse of
        Nothing -> fail "listenChan is closed and empty"
        Just response@(Message (HeadersMessage _) _) -> (handleResponse response)
        Just response -> (handleResponse response) >> handleMessages
      

getHeaders :: Connection ()
getHeaders = do
  connectionContext <- State.get
  config <- ask
  let
    (ConnectionContext
     { version' = version'
     , network' = network'
     , writerChan = writerChan
     , lastBlock' = lastBlock
     }) = connectionContext
    getHeadersMessage lastBlock = do
      blockLocatorHashes <- queryBlockLocatorHashes lastBlock config
      return $ Message
        (GetHeadersMessage version' blockLocatorHashes
         (BlockHash . fst . decode $ "0000000000000000000000000000000000000000000000000000000000000000"))
        (MessageContext network')
  message <- liftIO $ getHeadersMessage (fromIntegral lastBlock)
  liftIO . atomically $ writeTBMChan writerChan message


queryBlockLocatorHashes :: Int -> Config -> IO [BlockHash]
queryBlockLocatorHashes lastBlock (Config {pool=pool}) = do
  mapM queryBlockHash (blockLocatorIndices lastBlock)
  where
    queryBlockHash i = (hashBlock . decodeBlockHeader . fromJust) <$> runSqlPool (DB.get (toSqlKey . fromIntegral $  i + 1)) pool
    -- NOTE: we query by i + 1 since the genesis block (block 0) is in the db at index 1

blockLocatorIndices :: Int -> [Int]
blockLocatorIndices lastBlock = reverse . addGenesisIndiceIfNeeded $ blockLocatorIndicesStep 10 1 [lastBlock]
  where addGenesisIndiceIfNeeded (0:xs) = 0:xs
        addGenesisIndiceIfNeeded (xs)   = 0:xs

blockLocatorIndicesStep c step (i:is)
  | c > 0 && i > 0 = blockLocatorIndicesStep (c - 1) (step) ((i - 1):i:is)
  | i - step > 0 = blockLocatorIndicesStep c (step * 2) ((i - step):i:is)
  | otherwise = i:is
