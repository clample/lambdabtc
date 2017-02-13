{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol.Server where

import Protocol.Messages (parseMessage, Message(..), MessageBody(..), MessageContext(..))
import Protocol.MessageBodies 
import Protocol.Network (Peer(..), connectToPeer, sock, addr, Addr(..))
import BitcoinCore.BlockHeaders ( BlockHash(..)
                                , encodeBlockHeader
                                , BlockHeader(..)
                                , decodeBlockHeader
                                , hashBlock
                                , genesisBlock
                                , verifyHeaders)
import BitcoinCore.BloomFilter ( pDefault
                               , blankFilter
                               , updateFilter
                               , numberHashFunctions
                               , filterSize
                               , hardcodedTweak
                               , NFlags(..))
import General.Config (ConfigM(..), Config(..), pool)
import General.Persistence (runDB, PersistentBlockHeader(..))
import General.Types (HasNetwork(..), HasVersion(..), HasRelay(..), HasTime(..), HasLastBlock(..))

import Network.Socket (Socket)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Monad.State.Lazy (StateT(..), runStateT, liftIO)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad (when)
import qualified Control.Monad.State.Lazy as State
import System.Random (randomR, StdGen, getStdGen)
import Conduit (runConduit, (.|), mapC, mapMC, Conduit)
import Data.Conduit.Network (sourceSocket, sinkSocket)
import Data.Conduit.Serialization.Binary (conduitGet, conduitPut)
import Data.Conduit.TMChan ( sourceTBMChan
                           , sinkTBMChan
                           , newTBMChan
                           , TBMChan
                           , writeTBMChan
                           , readTBMChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Data.Binary (Binary(..))
import Data.ByteString.Base16 (decode)
import Database.Persist.Sql (insertMany_, count, runSqlPool, Filter, toSqlKey, insert_)
import qualified Database.Persist.Sql as DB
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Control.Lens (makeLenses, (^.), (+=))

data ConnectionContext = ConnectionContext
  { _connectionContextVersion :: Int
  , _connectionContextLastBlock :: Integer
  , _myAddr :: Addr
  , _peer :: Peer
  , _connectionContextRelay :: Bool
    -- https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#extensions-to-existing-messages
    -- Relay should be set to False when functioning as an SPV node
  , _writerChan :: TBMChan Message
  , _listenChan :: TBMChan Message
  , _connectionContextTime :: POSIXTime
  , _randGen :: StdGen
  } 

makeLenses ''ConnectionContext

instance HasVersion ConnectionContext where
  version = connectionContextVersion

instance HasRelay ConnectionContext where
  relay = connectionContextRelay

instance HasTime ConnectionContext where
  time = connectionContextTime

instance HasLastBlock ConnectionContext where
  lastBlock = connectionContextLastBlock

type Connection a = StateT ConnectionContext ConfigM a

runConnection :: Connection a -> ConnectionContext -> Config -> IO (a, ConnectionContext)
runConnection connectionM state =
  runReaderT (runConfigM (runStateT connectionM state))

connectTestnet :: Config -> IO () 
connectTestnet config = do
  persistGenesisBlock config
  context <- getConnectionContext config
  let peerSocket = context^.peer.sock
  forkIO $ listener (context^.listenChan) peerSocket
  forkIO $ writer (context^.writerChan) peerSocket
  runConnection connection context config
  return ()

getConnectionContext :: Config -> IO ConnectionContext
getConnectionContext config = do
  peer' <- connectToPeer 1
  writerChan' <- atomically $ newTBMChan 16
  listenChan' <- atomically $ newTBMChan 16
  time' <- getPOSIXTime
  randGen' <- getStdGen
  lastBlock' <- fromIntegral <$> getLastBlock config
  return ConnectionContext
        { _connectionContextVersion = 60002
        , _connectionContextLastBlock = lastBlock'
        , _myAddr = Addr (0, 0, 0, 0) 18333 
        , _peer = peer'
        , _connectionContextRelay = False 
        , _writerChan = writerChan'
        , _listenChan = listenChan'
        , _connectionContextTime = time'
        , _randGen = randGen'
        }

getLastBlock :: Config -> IO Int
getLastBlock config =
  ((-1) +) <$> runSqlPool lastBlockQuery (config^.pool)
  where lastBlockQuery = count allBlocksFilter
        allBlocksFilter = [] :: [Filter PersistentBlockHeader]

persistGenesisBlock :: Config -> IO ()
persistGenesisBlock config = do
  lastBlock' <- getLastBlock config
  when (lastBlock' == -1) $
    runSqlPool (insert_ . encodeBlockHeader . genesisBlock $ (config^.network)) (config^.pool)

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


logMessages :: Show b => String -> Conduit b IO b
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
  context <- State.get
  mResponse <-  liftIO . atomically . readTBMChan $ (context^.listenChan)
  case mResponse of
    Nothing -> fail "listenChan is closed and empty"
    Just response -> handleResponse response
  connectionLoop


handleResponse :: Message -> Connection ()

handleResponse (Message (VersionMessageBody body) _) = do
  context <- State.get
  config  <- ask
  let verackMessage =
        Message (VerackMessageBody VerackMessage) (MessageContext (config^.network))
      lastBlockPeer = body^.lastBlock
      lastBlockSelf = context^.lastBlock
  liftIO . atomically $ writeTBMChan (context^.writerChan) verackMessage
  when (lastBlockPeer > lastBlockSelf) $
    synchronizeHeaders lastBlockPeer

handleResponse (Message (PingMessageBody _) _) = do
  context <- State.get
  config  <- ask
  let pongMessage =
        Message (PongMessageBody PongMessage) (MessageContext (config^.network))
  liftIO . atomically $ writeTBMChan (context^.writerChan) pongMessage

handleResponse (Message (HeadersMessageBody (HeadersMessage headers)) _) = do
  let persistentHeaders = map encodeBlockHeader headers
      chunkedPersistentHeaders = chunksOf 100 persistentHeaders
      -- Headers are inserted in chunks
      -- sqlite rejects if we insert all at once
  mostRecentHeader <- getMostRecentHeader
  let isValid = verifyHeaders (mostRecentHeader:headers)
      newHeaders = fromIntegral . length $ headers
  if isValid
    then do
      runDB $ mapM_ insertMany_ chunkedPersistentHeaders
      lastBlock += newHeaders
    else fail "We recieved invalid headers"
    
handleResponse _ = return ()

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
  context <- State.get
  config <- ask
  let nonce' = fst $ randomR (0, 0xffffffffffffffff ) (context^.randGen)
      versionMessage = Message
          (VersionMessageBody (VersionMessage
            (context^.version)
            nonce'
            (context^.lastBlock)
            (context^.peer.addr)
            (context^.myAddr)
            (context^.relay)
            (context^.time)))
          (MessageContext (config^.network))
  liftIO . atomically $ writeTBMChan (context^.writerChan) versionMessage


setFilter :: Connection ()
setFilter = do
  context <- State.get
  config <- ask
  let
    blank = blankFilter 1 pDefault
    s = filterSize 1 pDefault
    nHashFuncs = numberHashFunctions s 1
    txId = fst . decode $ "b73619d208b4f7b91cc93185b1e2f5057bacbe9b5c0b63c36159e0354be0a77f"
    filter' = updateFilter nHashFuncs hardcodedTweak txId blank
    filterloadMessage = Message
      (FilterloadMessageBody (FilterloadMessage filter' nHashFuncs hardcodedTweak BLOOM_UPDATE_NONE))
      (MessageContext (config^.network))
  liftIO . atomically $ writeTBMChan (context^.writerChan) filterloadMessage


synchronizeHeaders :: Integer -> Connection ()
synchronizeHeaders lastBlockPeer = do
  context <- State.get
  when ((context^.lastBlock) < lastBlockPeer) $ do
    getHeaders
    handleMessages
    synchronizeHeaders lastBlockPeer
  where
    -- Keep reading messages until we get a headers message
    handleMessages = do
      context  <- State.get
      mResponse <-  liftIO . atomically . readTBMChan $ (context^.listenChan)
      case mResponse of
        Nothing -> fail "listenChan is closed and empty"
        Just response@(Message (HeadersMessageBody _) _) -> handleResponse response
        Just response -> handleResponse response >> handleMessages
      


getHeaders :: Connection ()
getHeaders = do
  context <- State.get
  config <- ask
  let getHeadersMessage lastBlock' = do
        blockLocatorHashes <- queryBlockLocatorHashes lastBlock' config
        return $ Message
          (GetHeadersMessageBody (GetHeadersMessage (context^.version) blockLocatorHashes
            (BlockHash . fst . decode $
             "0000000000000000000000000000000000000000000000000000000000000000")))
          (MessageContext (config^.network))
  message <- liftIO $ getHeadersMessage (fromIntegral (context^.lastBlock))
  liftIO . atomically $ writeTBMChan (context^.writerChan) message


queryBlockLocatorHashes :: Int -> Config -> IO [BlockHash]
queryBlockLocatorHashes lastBlock' config =
  mapM queryBlockHash (blockLocatorIndices lastBlock')
  where
    queryBlockHash i =
      (hashBlock . decodeBlockHeader . fromJust) <$>
        runSqlPool (DB.get (toSqlKey . fromIntegral $  i + 1)) (config^.pool)
    -- NOTE: we query by i + 1 since the genesis block (block 0) is in the db at index 1

blockLocatorIndices :: Int -> [Int]
blockLocatorIndices lastBlock' = reverse . addGenesisIndiceIfNeeded $ blockLocatorIndicesStep 10 1 [lastBlock']
  where addGenesisIndiceIfNeeded (0:xs) = 0:xs
        addGenesisIndiceIfNeeded xs   = 0:xs

blockLocatorIndicesStep :: Int -> Int -> [Int] -> [Int]
blockLocatorIndicesStep c step (i:is)
  | c > 0 && i > 0 = blockLocatorIndicesStep (c - 1) step ((i - 1):i:is)
  | i - step > 0 = blockLocatorIndicesStep c (step * 2) ((i - step):i:is)
  | otherwise = i:is
blockLocatorIndicesStep _ _ [] =
  error "blockLocatorIndicesStep must be called with a nonempty accummulator array "
