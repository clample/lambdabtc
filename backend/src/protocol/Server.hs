{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol.Server where

import Protocol.Messages (parseMessage, Message(..), MessageBody(..), MessageContext(..))
import Protocol.MessageBodies 
import Protocol.Network (connectToPeer, sock, addr, Addr(..))
import Protocol.Util (decodeBlockHeader, getUTXOS)
import Protocol.Persistence ( getLastBlock
                            , persistGenesisBlock
                            , persistHeaders
                            , persistHeader
                            , firstHeaderMatch
                            , getBlockWithIndex
                            , nHeadersSince
                            , getHeaderFromEntity
                            , getAllAddresses
                            , isTransactionHandled
                            , persistUTXOs
                            , persistTransaction)
import Protocol.ConnectionM ( ConnectionContext(..)
                            , myAddr
                            , peer
                            , writerChan
                            , listenChan
                            , randGen
                            , Connection
                            , runConnection)
import BitcoinCore.BlockHeaders ( BlockHash(..)
                                , BlockHeader(..)
                                , verifyHeaders)
import BitcoinCore.BloomFilter ( NFlags(..)
                               , defaultFilterWithElements)
import BitcoinCore.Keys (PubKeyHash(..), addressToPubKeyHash)
import BitcoinCore.Inventory (InventoryVector(..), ObjectType(..))
import General.Config (Config(..), appChan)
import General.Persistence (runDB, PersistentBlockHeader(..), FundRequest(..))
import General.Types (HasNetwork(..), HasVersion(..), HasRelay(..), HasTime(..), HasLastBlock(..))
import General.InternalMessaging (InternalMessage(..))

import Network.Socket (Socket)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.State.Lazy (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad (when, filterM)
import qualified Control.Monad.State.Lazy as State
import System.Random (randomR, getStdGen)
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
import Data.Binary.Put (runPut)
import Data.ByteString.Base16 (decode, encode)
import Data.ByteString.Lazy (toStrict)
import Database.Persist.Sql ( count
                            , Filter)
import Data.Maybe (fromJust)
import Control.Lens ((^.), (+=))

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
  peer' <- connectToPeer 1 config
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
  config  <- ask
  let verackMessage =
        Message (VerackMessageBody VerackMessage) (MessageContext (config^.network))
      lastBlockPeer = body^.lastBlock
  writeMessage verackMessage
  synchronizeHeaders lastBlockPeer

handleResponse (Message (PingMessageBody message) _) = do
  config  <- ask
  let pongMessageBody = PongMessageBody . PongMessage $ message^.nonce64
      pongMessage =
        Message pongMessageBody (MessageContext (config^.network))
  writeMessage pongMessage

handleResponse (Message (HeadersMessageBody (HeadersMessage headers)) _) = do
  mostRecentHeader <- getMostRecentHeader
  let isValid = verifyHeaders (mostRecentHeader:headers)
      newHeadersN = fromIntegral . length $ headers
  if isValid
    then do
      persistHeaders headers
      lastBlock += newHeadersN
    else fail "We recieved invalid headers"

handleResponse (Message (MerkleblockMessageBody (message)) _) = do
  mostRecentHeader <- getMostRecentHeader
  let isValid = verifyHeaders [mostRecentHeader, (message^.blockHeader)]
  if isValid
    then do
      persistHeader $ message^.blockHeader
      lastBlock += 1
    else fail "We recieved invalid header"

handleResponse (Message (GetHeadersMessageBody message) _) = do
  config <- ask
  match <- firstHeaderMatch (message^.blockLocatorHashes)
  matchingHeaderEntities <- 2000 `nHeadersSince` match
  let matchingHeaders = map getHeaderFromEntity matchingHeaderEntities
      headersMessage =
        Message
        (HeadersMessageBody (HeadersMessage {_blockHeaders = matchingHeaders}))
        (MessageContext (config^.network))
  writeMessage headersMessage
  
handleResponse (Message (InvMessageBody message) _) = do
  config <- ask
  desiredInvs <- map toFilteredBlock <$> filterM (desiredData) (message^.invVectors)
  let getDataMessage =
        Message
        (GetDataMessageBody (GetDataMessage desiredInvs))
        (MessageContext (config^.network))
  writeMessage getDataMessage
  where
    -- TODO: query db, etc to see if we need the data
    desiredData _ = return True
    toFilteredBlock (InventoryVector MSG_BLOCK hash) = InventoryVector MSG_FILTERED_BLOCK hash
    toFilteredBlock invVector = invVector

-- TODO: two threads may see isHandled False
--       and then go on to persist duplicate UTXOs
handleResponse (Message (TxMessageBody message) _) = do
  isHandled <- isTransactionHandled (message^.transaction)
  when (not isHandled) $ do
    addUTXOS
    persistTransaction (message^.transaction)
  where
    addUTXOS = do
      let getPubKeyHashBS (PubKeyHash bs) = bs
      pubKeyHashes <- map (getPubKeyHashBS . addressToPubKeyHash) <$> getAllAddresses
      let indexedPubkeyHashes = zip [1..] pubKeyHashes
      let persistentUTXOs = getUTXOS indexedPubkeyHashes (message^.transaction)
      persistUTXOs persistentUTXOs
  

handleResponse _ = return ()

handleInternalMessage :: InternalMessage -> Connection ()
handleInternalMessage (SendTX transaction') = do
  liftIO . putStrLn $ "Sending transaction " ++
    (show . encode . toStrict . runPut . put) transaction'
  config <- ask
  let body = TxMessageBody . TxMessage $ transaction'
      messageContext = MessageContext (config^.network)
      txMessage = Message body messageContext
  writeMessage txMessage
handleInternalMessage (AddAddress address) = do
  config <- ask
  let getPubKeyHashBS (PubKeyHash bs) = bs
      body = FilteraddMessageBody
             . FilteraddMessage
             . getPubKeyHashBS
             . addressToPubKeyHash
             $ address
      messageContext = MessageContext (config ^. network)
      filterAddMessage = Message body messageContext
  writeMessage filterAddMessage

getMostRecentHeader :: Connection BlockHeader
getMostRecentHeader = do
  config <- ask
  blockHeaderCount <- liftIO $ fromIntegral <$> getLastBlock config
  mlastBlockHeader <- getBlockWithIndex blockHeaderCount
  case mlastBlockHeader of
    Nothing -> fail "Unable to get most recent block header. This should never happen"
    Just lastBlockHeader -> return (decodeBlockHeader lastBlockHeader)

sendVersion :: Connection ()
sendVersion = do
  context <- State.get
  config <- ask
  let nonce' = Nonce64 . fst $ randomR (0, 0xffffffffffffffff ) (context^.randGen)
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
  writeMessage versionMessage


setFilter :: Connection ()
setFilter = do
  config <- ask
  let getPubKeyHashBS (PubKeyHash bs) = bs
  pubKeyHashes <- map (getPubKeyHashBS . addressToPubKeyHash) <$> getAllAddresses
  let
    (filter', filterContext') = defaultFilterWithElements pubKeyHashes
    filterloadMessage = Message
      (FilterloadMessageBody (FilterloadMessage filter' filterContext' BLOOM_UPDATE_NONE))
      (MessageContext (config^.network))
  writeMessage filterloadMessage


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
      handleMessages
      synchronizeHeaders lastBlockPeer
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
  let getHeadersMessage' lastBlock' = do
        blockLocatorHashes' <- queryBlockLocatorHashes lastBlock'
        return $ Message
          (bodyConstructor (messageConstructor (context^.version) blockLocatorHashes'
            (BlockHash . fst . decode $
             "0000000000000000000000000000000000000000000000000000000000000000")))
          (MessageContext (config^.network))
  message <- getHeadersMessage' (fromIntegral (context^.lastBlock))
  writeMessage message

queryBlockLocatorHashes :: Int -> Connection [BlockHash]
queryBlockLocatorHashes lastBlock' =
  mapM queryBlockHash (blockLocatorIndices lastBlock')
  where
    queryBlockHash i =
      (BlockHash . persistentBlockHeaderHash . fromJust) <$> getBlockWithIndex i

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
  where allKeysFilter = [] :: [ Filter FundRequest ]
  -- When there are no fund requests,
  -- we can assume that no transactions have been sent to our addresses

writeMessage :: Message -> Connection ()
writeMessage message = do
  context <- State.get
  liftIO . atomically $ writeTBMChan (context^.writerChan) message
