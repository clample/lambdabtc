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
import Control.Monad.Reader (runReaderT)
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
import Database.Persist.Sql (insertMany_, count, runSqlPool, Filter, toSqlKey)
import qualified Database.Persist.Sql as DB
import Persistence (runDB, PersistentBlockHeader(..))
import Data.List.Split (chunksOf)
import BlockHeaders (BlockHeader(..), decodeBlockHeader)
import Data.Maybe (catMaybes)


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
  context <- getConnectionContext config
  let listenChan' = listenChan context
      writerChan' = writerChan context
      Peer peerSocket _ = peer' context
  forkIO $ listener listenChan' peerSocket
  forkIO $ writer writerChan' peerSocket
  getBlockLocatorHashes 160 config >>= print
  -- runConnection connection context config
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
  runSqlPool lastBlockQuery pool
  where lastBlockQuery = count allBlocksFilter
        allBlocksFilter = [] :: [Filter PersistentBlockHeader]

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
  getHeaders
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

handleResponse (Message (VersionMessage {}) _) = do
  connectionContext <- State.get
  let
    network = network' connectionContext
    writerChan' = writerChan connectionContext
    verackMessage = Message VerackMessage (MessageContext network)
  liftIO . atomically $ writeTBMChan writerChan' verackMessage

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
  runDB $ mapM_ insertMany_ chunkedPersistentHeaders

handleResponse _ = do
  return ()

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

getHeaders :: Connection ()
getHeaders = do
  connectionContext <- State.get
  let
    (ConnectionContext
     { version' = version'
     , network' = network'
     , writerChan = writerChan
     }) = connectionContext
    getHeadersMessage = Message
        (GetHeadersMessage version' [genesisHash network'] (BlockHash . fst . decode $ "0000000000000000000000000000000000000000000000000000000000000000"))
        (MessageContext network')
  liftIO . atomically $ writeTBMChan writerChan getHeadersMessage


getBlockLocatorHashes :: Int -> Config -> IO [BlockHash]
getBlockLocatorHashes i c = (++ [genesisHash TestNet3]) <$> blockHashes 
  where blockHashes = map (headerToHash . decodeBlockHeader) . catMaybes <$> queryBlockLocatorHeaders i c
        headerToHash (BlockHeader _ blockHash _ _ _ _ _) = blockHash
  
queryBlockLocatorHeaders :: Int -> Config -> IO [Maybe PersistentBlockHeader]
queryBlockLocatorHeaders lastBlock (Config {pool=pool}) = do
  mapM queryBlockHash (blockLocatorIndices lastBlock)
  where
    queryBlockHash i = runSqlPool (DB.get (toSqlKey . fromIntegral $  i)) pool

blockLocatorIndices :: Int -> [Int]
blockLocatorIndices lastBlock = reverse $ blockLocatorIndicesStep 10 1 [lastBlock]

blockLocatorIndicesStep c step (i:is)
  | c > 0 && i > 0 = blockLocatorIndicesStep (c - 1) (step) ((i - 1):i:is)
  | i - step > 0 = blockLocatorIndicesStep c (step * 2) ((i - step):i:is)
  | otherwise = i:is
