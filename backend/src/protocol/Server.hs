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
                            , getBlockWithIndex
                            , getAllAddresses
                            , persistUTXOs
                            , persistTransaction
                            , getBlockHeaderFromHash
                            , nHeadersSinceKey
                            , getTransactionFromHash)
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
                                , verifyHeaders
                                , hashBlock)
import BitcoinCore.BloomFilter ( NFlags(..)
                               , defaultFilterWithElements)
import BitcoinCore.Keys (PubKeyHash(..), addressToPubKeyHash, Address(..))
import BitcoinCore.Inventory (InventoryVector(..), ObjectType(..))
import BitcoinCore.Transaction.Transactions (Value(..), Transaction(..), TxHash(..), hashTransaction)
import General.Config (Config(..), appChan, uiUpdaterChan, pool)
import General.Persistence (runDB, PersistentBlockHeader(..), FundRequest(..), PersistentUTXO(..))
import General.Types (HasNetwork(..), HasVersion(..), HasRelay(..), HasTime(..), HasLastBlock(..))
import General.InternalMessaging (InternalMessage(..), UIUpdaterMessage(..))

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
import Data.ByteString.Base16 (decode)
import Control.Lens ((^.), (+=))
import Control.Monad.Free (Free(..), liftF)

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
  lastBlock' <- fromIntegral <$> getLastBlock (config^.pool)
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
  interpretConnProd sendVersion'
  interpretConnProd setFilter'
  connectionLoop

connectionLoop :: Connection ()
connectionLoop = do
  context <- State.get
  config <- ask
  mmResponse <-  liftIO . atomically . tryReadTBMChan $ (context^.listenChan)
  case mmResponse of
    Nothing -> fail "listenChan is closed and empty"
    Just Nothing -> return () -- chan is empty
    Just (Just response) -> interpretConnProd $ handleResponse' response
  mmInternalMessage <- liftIO . atomically . tryReadTBMChan $ (config^.appChan)
  case mmInternalMessage of
    Nothing -> fail "appChan is closed and empty"
    Just Nothing -> return () -- chan is empty
    Just (Just internalMessage) -> interpretConnProd $ handleInternalMessage' internalMessage
  connectionLoop

----------
type KeyId = Integer

data ConnectionInteraction next
  = GetConfig (Config -> next)
  | GetContext (ConnectionContext -> next)
  | IncrementLastBlock Integer next
  | ReadMessage (Maybe Message -> next)
  | WriteMessage Message next
  | WriteUIUpdaterMessage UIUpdaterMessage next
  | GetBlockHeader KeyId (Maybe BlockHeader -> next)
  | BlockHeaderCount (Int -> next)
  | PersistBlockHeaders [BlockHeader] next
  | PersistBlockHeader BlockHeader next
  | GetBlockHeaderFromHash BlockHash (Maybe (KeyId, BlockHeader) -> next)
  | PersistTransaction Transaction next
  | NHeadersSinceKey Int KeyId ([BlockHeader] -> next)
  | GetTransactionFromHash TxHash (Maybe KeyId -> next)
  | GetAllAddresses ([Address] -> next)
  | PersistUTXOs [PersistentUTXO] next

instance Functor ConnectionInteraction where
  fmap f (GetConfig                 g) = GetConfig                 (f . g)
  fmap f (GetContext                g) = GetContext                (f . g)
  fmap f (IncrementLastBlock      i x) = IncrementLastBlock      i (f x)
  fmap f (ReadMessage               g) = ReadMessage               (f . g)
  fmap f (WriteMessage            m x) = WriteMessage            m (f x)
  fmap f (WriteUIUpdaterMessage   m x) = WriteUIUpdaterMessage   m (f x)
  fmap f (GetBlockHeader          i g) = GetBlockHeader          i (f . g)
  fmap f (BlockHeaderCount          g) = BlockHeaderCount          (f . g)
  fmap f (PersistBlockHeaders     h x) = PersistBlockHeaders     h (f x)
  fmap f (PersistBlockHeader      h x) = PersistBlockHeader      h (f x)
  fmap f (GetBlockHeaderFromHash  h g) = GetBlockHeaderFromHash  h (f . g)
  fmap f (PersistTransaction      t x) = PersistTransaction      t (f x)
  fmap f (NHeadersSinceKey        n i g) = NHeadersSinceKey         n i (f . g)
  fmap f (GetTransactionFromHash  h g) = GetTransactionFromHash  h (f . g)
  fmap f (GetAllAddresses           g) = GetAllAddresses           (f . g)
  fmap f (PersistUTXOs            u x) = PersistUTXOs            u (f x)

type Connection' = Free ConnectionInteraction

-- syntactic sugar to enable `do` notation
getConfig' :: Connection' Config
getConfig' = liftF (GetConfig id)

getContext' :: Connection' ConnectionContext
getContext' = liftF (GetContext id)

-- TODO: Can we move this out of the interpreter and into our free monad
--       and just expose a generic way to update the state?
incrementLastBlock' :: Integer -> Connection' ()
incrementLastBlock' i = liftF (IncrementLastBlock i ())

readMessage' :: Connection' (Maybe Message)
readMessage' = liftF (ReadMessage id)

writeMessage' :: Message -> Connection' ()
writeMessage' message = liftF (WriteMessage message ())

writeUiUpdaterMessage' :: UIUpdaterMessage -> Connection' ()
writeUiUpdaterMessage' message = liftF (WriteUIUpdaterMessage message ())

getBlockHeader' :: KeyId -> Connection' (Maybe BlockHeader)
getBlockHeader' index = liftF (GetBlockHeader index id)

blockHeaderCount' :: Connection' Int
blockHeaderCount' = liftF (BlockHeaderCount id)

persistHeaders' :: [BlockHeader] -> Connection' ()
persistHeaders' headers = liftF (PersistBlockHeaders headers ())

persistHeader' :: BlockHeader -> Connection' ()
persistHeader' header = liftF (PersistBlockHeader header ())

getBlockHeaderFromHash' :: BlockHash -> Connection' (Maybe (Integer, BlockHeader))
getBlockHeaderFromHash' hash = liftF (GetBlockHeaderFromHash hash id)

persistTransaction' :: Transaction -> Connection' ()
persistTransaction' tx = liftF (PersistTransaction tx ())

nHeadersSinceKey' :: Int -> KeyId -> Connection' [BlockHeader]
nHeadersSinceKey' n keyId = liftF (NHeadersSinceKey n keyId id)

getTransactionFromHash' :: TxHash -> Connection' (Maybe Integer)
getTransactionFromHash' hash = liftF (GetTransactionFromHash hash id)

getAllAddresses' :: Connection' [Address]
getAllAddresses' = liftF (GetAllAddresses id)

persistUTXOs' :: [PersistentUTXO] -> Connection' ()
persistUTXOs' persistentUTXOs = liftF (PersistUTXOs persistentUTXOs ())

--
-- TODO: It should be possible to get rid of our Connection monad stack / State and Reader
--       See "freer monads" paper
interpretConnProd :: Connection' r -> Connection r
interpretConnProd conn = case conn of
  Free (GetConfig f) -> do
    config <- ask
    interpretConnProd (f config)
  Free (GetContext f) -> do
    context <- State.get
    interpretConnProd (f context)
  Free (IncrementLastBlock i n) -> do
    lastBlock += i
    interpretConnProd n
  Free (ReadMessage f) -> do
    context <- State.get
    mMessage <- liftIO . atomically . readTBMChan $ (context^.listenChan)
    interpretConnProd (f mMessage)
  Free (WriteMessage m n) -> do
    context <- State.get
    liftIO $ writeMessage (context^.writerChan) m 
    interpretConnProd n
  Free (WriteUIUpdaterMessage m n) -> do
    config <- ask
    liftIO $ writeUiUpdaterMessage (config^.uiUpdaterChan) m
    interpretConnProd n
  Free (GetBlockHeader i f) -> do
    config <- ask
    mBlock <- liftIO $ (getBlockWithIndex (config^.pool) i)
    interpretConnProd (f mBlock)
  Free (BlockHeaderCount f) -> do
    config <- ask
    blockHeaderCount <- liftIO . getLastBlock $ config^.pool
    interpretConnProd (f blockHeaderCount)
  Free (PersistBlockHeaders headers n) -> do
    config <- ask
    liftIO $ persistHeaders (config^.pool) headers
    interpretConnProd n
  Free (PersistBlockHeader header n) -> do
    config <- ask
    liftIO $ persistHeader (config^.pool) header
    interpretConnProd n
  Free (GetBlockHeaderFromHash hash f) -> do
    config <- ask
    mBlock <- liftIO $ getBlockHeaderFromHash (config^.pool) hash
    interpretConnProd (f mBlock)
  Free (NHeadersSinceKey n keyId f) -> do
    config <- ask
    headers <- liftIO $ nHeadersSinceKey (config^.pool) n keyId
    interpretConnProd (f headers)
  Free (PersistTransaction tx n) -> do
    config <- ask
    liftIO $ persistTransaction (config^.pool) tx
    interpretConnProd n
  Free (GetTransactionFromHash hash f) -> do
    config <- ask
    mTx <- liftIO $ getTransactionFromHash (config^.pool) hash
    interpretConnProd (f mTx)
  Free (GetAllAddresses f) -> do
    config <- ask
    addresses <- liftIO $ getAllAddresses (config^.pool)
    interpretConnProd (f addresses)
  Free (PersistUTXOs utxos n) -> do
    config <- ask
    liftIO $ persistUTXOs (config^.pool) utxos
    interpretConnProd n
  Pure r -> return r

-- Logic for handling response in free monad
handleResponse' :: Message -> Connection' ()
handleResponse' (Message (PingMessageBody message) _) = do
  config <- getConfig'
  let pongMessageBody = PongMessageBody . PongMessage $ message^.nonce64
      pongMessage =
        Message pongMessageBody (MessageContext (config^.network))
  writeMessage' pongMessage

handleResponse' (Message (VersionMessageBody body) _) = do
  config <- getConfig'
  let verackMessage =
        Message (VerackMessageBody VerackMessage) (MessageContext (config^.network))
      lastBlockPeer = body^.lastBlock
  writeMessage' verackMessage
  synchronizeHeaders' lastBlockPeer

handleResponse' (Message (HeadersMessageBody (HeadersMessage headers)) _) = do
  mostRecentHeader <- getMostRecentHeader'
  let isValid = verifyHeaders (mostRecentHeader:headers)
      newHeadersN = fromIntegral . length $ headers
  if isValid
    then do
      persistHeaders' headers
      incrementLastBlock' newHeadersN
    else fail "We recieved invalid headers"
         -- todo: does `fail` actually make sense here?

handleResponse' (Message (MerkleblockMessageBody (message)) _) = do
  mostRecentHeader <- getMostRecentHeader'
  let isValid = verifyHeaders [mostRecentHeader, (message^.blockHeader)]
  if isValid
    then do
      persistHeader' $ message^.blockHeader
      incrementLastBlock' 1
    else fail "We recieved invalid header"
      -- todo: does `fail` actually make sense here?

handleResponse' (Message (GetHeadersMessageBody message) _) = do
  config <- getConfig'
  match <- firstHeaderMatch' (message^.blockLocatorHashes)
  matchingHeaders <- 2000 `nHeadersSinceKey'` match
  let headersMessage =
        Message
         (HeadersMessageBody (HeadersMessage {_blockHeaders = matchingHeaders }))
         (MessageContext (config^.network))
  writeMessage' headersMessage

handleResponse' (Message (InvMessageBody message) _) = do
  config <- getConfig'
  desiredInvs <- map toFilteredBlock <$> filterM (desiredData) (message^.invVectors)
  let getDataMessage =
        Message
        (GetDataMessageBody (GetDataMessage desiredInvs))
        (MessageContext (config^.network))
  writeMessage' getDataMessage
  where
    -- TODO: query db, etc to see if we actually need the data
    desiredData _ = return True
    toFilteredBlock (InventoryVector MSG_BLOCK hash) = InventoryVector MSG_FILTERED_BLOCK hash
    toFilteredBlock invVector = invVector

-- TODO: If this is not atomic, then there is a race condition
--       two threads may see isHandled False
--       and then proceed to persist duplicate UTXOs
handleResponse' (Message (TxMessageBody message) _) = do
  isHandled <- isTransactionHandled'
  when (not isHandled) $ do
    addUTXOs
    writeUiUpdaterMessage' . IncomingFunds . Satoshis $ 1000
    persistTransaction' (message^.transaction)
  where
    addUTXOs = do
      let getPubKeyHashBS (PubKeyHash bs) = bs
      pubKeyHashes <- map (getPubKeyHashBS . addressToPubKeyHash) <$> getAllAddresses'
      let indexedPubkeyHashes = zip [1..] pubKeyHashes
          persistentUTXOs = getUTXOS indexedPubkeyHashes (message^.transaction)
      persistUTXOs' persistentUTXOs
    isTransactionHandled' = do
      let txHash = hashTransaction $ message^.transaction
      mTx <- getTransactionFromHash' txHash
      return $ case mTx of
        Nothing -> False
        Just _  -> True

handleResponse' message = error $
  "We are not yet able to handle message" ++ (show message)

-- returns the leftmose header that we are currently persisting
firstHeaderMatch' :: [BlockHash] -> Connection' Integer
firstHeaderMatch' [] = fail "No matching hash was found"
firstHeaderMatch' (hash:hashes) = do
  mHeader <- getBlockHeaderFromHash' hash
  case mHeader of
    Just (index, _) -> return index
    Nothing -> firstHeaderMatch' hashes

getMostRecentHeader' :: Connection' BlockHeader
getMostRecentHeader' = do
  blockHeaderCount <- blockHeaderCount'
  mLastBlockHeader <- getBlockHeader' . fromIntegral $ blockHeaderCount + 1
    -- We add 1 to the count, since the database has 1 based indexing but
    -- blockHeaderCount has 0 based indexing
  case mLastBlockHeader of
    Nothing -> fail "Unable to get most recent block header. This should never happen"
    Just lastBlockHeader -> return lastBlockHeader

synchronizeHeaders' :: Integer -> Connection' ()
synchronizeHeaders' lastBlockPeer = do
  context <- getContext'
  newSync <- isNewSync'
  when (outOfSync' context) $
    if newSync
    then do
      getHeaders'
      handleMessages'
      synchronizeHeaders' lastBlockPeer
    else do
      getBlocks'
      handleMessages'
      synchronizeHeaders' lastBlockPeer
  where
    outOfSync' context = context^.lastBlock < lastBlockPeer
    handleMessages' = do
      mResponse <- readMessage'
      case mResponse of
        Nothing -> fail "unable to read message"
        -- TODO: handleResponse' will fail for a lot of message types
        Just response@(Message (HeadersMessageBody _) _) -> handleResponse' response
        Just response -> handleResponse' response >> handleMessages'

getHeaders' :: Connection' ()
getHeaders' = getHeadersOrBlocksMessage' GetHeadersMessageBody GetHeadersMessage

getBlocks' :: Connection' ()
getBlocks' = getHeadersOrBlocksMessage' GetBlocksMessageBody GetBlocksMessage

-- getheaders and getblocks messages are very similar
-- we can take the type constructors as arguments and use this same function
-- to create both types of messages
getHeadersOrBlocksMessage' :: (a -> MessageBody)
                           -> (Int -> [BlockHash] -> BlockHash -> a)
                           -> Connection' ()
getHeadersOrBlocksMessage' bodyConstructor messageConstructor = do
  context <- getContext'
  config <- getConfig'
  let getHeadersMessage' lastBlock' = do
        blockLocatorHashes' <- queryBlockLocatorHashes' lastBlock'
        return $ Message
          (bodyConstructor (messageConstructor (context^.version) blockLocatorHashes'
           (BlockHash . fst . decode $
            "0000000000000000000000000000000000000000000000000000000000000000")))
          (MessageContext (config^.network))
  message <- getHeadersMessage'(fromIntegral (context^.lastBlock))
  writeMessage' message

-- returns a list of some of the block hashes for headers we've persisted
-- this is used by the peer to send us headers starting with where
-- we diverge from their main chain
queryBlockLocatorHashes' :: Int -> Connection' [BlockHash]
queryBlockLocatorHashes' lastBlock' =
  mapM queryBlockHash (blockLocatorIndices lastBlock')
  where
    queryBlockHash i = do
      mBlockHeader <- getBlockHeader' . fromIntegral $ i + 1
        -- We add 1 to the count, since the database has 1 based indexing but
        -- blockHeaderCount has 0 based indexing
      case mBlockHeader of
        Nothing -> fail $ "Unable to get block header with index " ++ show i
        Just header -> return . hashBlock $ header

isNewSync' :: Connection' Bool
isNewSync' = (== []) <$> getAllAddresses'
  -- When there are no addresses
  -- we can assume that no transactions have been sent to us

sendVersion' :: Connection' ()
sendVersion' = do
  context <- getContext'
  config <- getConfig'
  let nonce' = Nonce64 . fst $ randomR (0, 0xffffffffffffffff) (context^.randGen)
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
  writeMessage' versionMessage

setFilter' :: Connection' ()
setFilter' = do
  config <- getConfig'
  let getPubKeyHashBS (PubKeyHash bs) = bs
  pubKeyHashes <- map (getPubKeyHashBS . addressToPubKeyHash) <$> getAllAddresses'
  let
    (filter', filterContext') = defaultFilterWithElements pubKeyHashes
    filterloadMessage = Message
      (FilterloadMessageBody (FilterloadMessage filter' filterContext' BLOOM_UPDATE_NONE))
      (MessageContext (config^.network))
  writeMessage' filterloadMessage

handleInternalMessage' :: InternalMessage -> Connection' ()
handleInternalMessage' (SendTX transaction') = do
  config <- getConfig'
  let body = TxMessageBody . TxMessage $ transaction'
      messageContext = MessageContext (config^.network)
      txMessage = Message body messageContext
  writeMessage' txMessage

handleInternalMessage' (AddAddress address) = do
  config <- getConfig'
  let getPubKeyHashBS (PubKeyHash bs) = bs
      body = FilteraddMessageBody
           . FilteraddMessage
           . getPubKeyHashBS
           . addressToPubKeyHash
           $ address
      messageContext = MessageContext (config^.network)
      filterAddMessage = Message body messageContext
  writeMessage' filterAddMessage

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

writeMessage :: TBMChan Message -> Message -> IO ()
writeMessage chan message = 
  liftIO . atomically $ writeTBMChan chan message

writeUiUpdaterMessage :: TBMChan UIUpdaterMessage -> UIUpdaterMessage -> IO ()
writeUiUpdaterMessage chan message =
  liftIO . atomically $ writeTBMChan chan message
