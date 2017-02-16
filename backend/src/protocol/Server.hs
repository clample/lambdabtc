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
import Protocol.Util (encodeBlockHeader, decodeBlockHeader)
import BitcoinCore.BlockHeaders ( BlockHash(..)
                                , BlockHeader(..)
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
import BitcoinCore.Inventory (InventoryVector(..), ObjectType(..), ObjectHash(..))
import General.Config (ConfigM(..), Config(..), pool, appChan)
import General.Persistence (runDB, PersistentBlockHeader(..), KeySet(..), EntityField(..))
import General.Types (HasNetwork(..), HasVersion(..), HasRelay(..), HasTime(..), HasLastBlock(..))
import General.InternalMessaging (InternalMessage(..))

import Network.Socket (Socket)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Monad.State.Lazy (StateT(..), runStateT, liftIO)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad (when, filterM)
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
                           , readTBMChan
                           , tryReadTBMChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Data.Binary (Binary(..))
import Data.ByteString.Base16 (decode)
import Database.Persist.Sql ( insertMany_
                            , count
                            , runSqlPool
                            , Filter
                            , toSqlKey
                            , insert_
                            , selectList
                            , (==.)
                            , (>=.))
import qualified Database.Persist.Sql as DB
import Database.Persist.Types (SelectOpt(..))
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
  peer' <- connectToPeer 3
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
  config <- ask
  mmResponse <-  liftIO . atomically . tryReadTBMChan $ (context^.listenChan)
  case mmResponse of
    Nothing -> fail "listenChan is closed and empty"
    Just Nothing -> return () -- chan is empty
    Just (Just response) -> handleResponse response
  mmInternalMessage <- liftIO . atomically . tryReadTBMChan $ (config^.appChan)
  case mmInternalMessage of
    Nothing -> fail "appChan is closed and empty"
    Just Nothing -> return () -- chan is empty
    Just (Just internalMessage) -> handleInternalMessage internalMessage
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


handleResponse (Message (GetHeadersMessageBody message) _) = do
  config <- ask
  context <- State.get
  DB.Entity headerId _ <- firstHeaderMatch (message^.blockLocatorHashes)
  matchingHeaderEntities <- runDB $
    selectList [ PersistentBlockHeaderId >=. headerId ] [LimitTo 2000]
  let getHeaderFromEntity (DB.Entity _ persistentHeader) = decodeBlockHeader persistentHeader
      matchingHeaders = map getHeaderFromEntity matchingHeaderEntities
      headersMessage =
        Message
        (HeadersMessageBody (HeadersMessage {_blockHeaders = matchingHeaders}))
        (MessageContext (config^.network))
  liftIO . atomically $ writeTBMChan (context^.writerChan) headersMessage
  
handleResponse (Message (InvMessageBody message) _) = do
  config <- ask
  context <- State.get
  desiredInvs <- map toFilteredBlock <$> filterM (desiredData) (message^.invVectors)
  let getDataMessage =
        Message
        (GetDataMessageBody (GetDataMessage desiredInvs))
        (MessageContext (config^.network))
  liftIO . atomically $ writeTBMChan (context^.writerChan) getDataMessage
  where
    -- TODO: query db, etc to see if we need the data
    desiredData _ = return True
    toFilteredBlock (InventoryVector MSG_BLOCK hash) = InventoryVector MSG_FILTERED_BLOCK hash
    toFilteredBlock invVector = invVector

handleResponse _ = return ()

handleInternalMessage :: InternalMessage -> Connection ()
handleInternalMessage (SendTX transaction) = do
  context <- State.get
  config <- ask
  let body = TxMessageBody . TxMessage $ transaction
      messageContext = MessageContext (config^.network)
      txMessage = Message body messageContext
  liftIO . atomically $ writeTBMChan (context^.writerChan) txMessage


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
  newSync <- isNewSync
  when (outOfSync context) $
    if newSync
    then do
      getHeaders
      handleMessages
      synchronizeHeaders lastBlockPeer
    else do
      getBlocks
  where
    outOfSync context = (context^.lastBlock) < lastBlockPeer
    -- Keep reading messages until we get a headers message
    handleMessages = do
      context  <- State.get
      mResponse <-  liftIO . atomically . readTBMChan $ (context^.listenChan)
      case mResponse of
        Nothing -> fail "listenChan is closed and empty"
        Just response@(Message (HeadersMessageBody _) _) -> handleResponse response
        Just response -> handleResponse response >> handleMessages


firstHeaderMatch :: [BlockHash] -> Connection (DB.Entity PersistentBlockHeader)
firstHeaderMatch [] = fail "No matching hash was found"
firstHeaderMatch (hash:hashes) = do
  mHeader <- haveHeader hash
  case mHeader of
    Just header -> return header
    Nothing -> firstHeaderMatch hashes

haveHeader :: BlockHash -> Connection (Maybe (DB.Entity PersistentBlockHeader))
haveHeader (BlockHash hash) = do
  matches <- runDB $ selectList [PersistentBlockHeaderHash ==. hash] []
  case matches of
    []       -> return Nothing
    [header] -> return $ Just header
    _        -> fail "Multiple blocks found with same hash"


getHeaders :: Connection ()
getHeaders = getHeadersOrBlocksMessage GetHeadersMessageBody GetHeadersMessage

getBlocks :: Connection ()
getBlocks = getHeadersOrBlocksMessage GetBlocksMessageBody GetBlocksMessage

getHeadersOrBlocksMessage :: (a -> MessageBody) 
                          -> (Int -> [BlockHash] -> BlockHash -> a)
                          -> Connection ()
getHeadersOrBlocksMessage bodyConstructor messageConstructor = do
  context <- State.get
  config <- ask
  let getHeadersMessage lastBlock' = do
        blockLocatorHashes <- queryBlockLocatorHashes lastBlock' config
        return $ Message
          (bodyConstructor (messageConstructor (context^.version) blockLocatorHashes
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

isNewSync :: Connection Bool
isNewSync = (== 0) <$> runDB (count allKeysFilter)
  where allKeysFilter = [] :: [ Filter KeySet ]
