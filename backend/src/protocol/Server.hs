{-# LANGUAGE OverloadedStrings #-}
module Protocol.Server where

import Protocol.Messages (parseMessage, Message(..), MessageBody(..), MessageContext(..))
import Protocol.MessageBodies 
import Protocol.Network (connectToPeer, sock, addr)
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
                            , IOHandlers(..)
                            , peerSocket
                            , myAddr
                            , writerChan
                            , listenChan
                            , randGen)
import BitcoinCore.BlockHeaders ( BlockHash(..)
                                , BlockHeader(..)
                                , verifyHeaders
                                , hashBlock)
import BitcoinCore.BloomFilter ( NFlags(..)
                               , defaultFilterWithElements)
import BitcoinCore.Keys (PubKeyHash(..), addressToPubKeyHash, Address(..))
import BitcoinCore.Inventory (InventoryVector(..), ObjectType(..))
import BitcoinCore.Transaction.Transactions (Value(..), Transaction(..), TxHash(..), hashTransaction)
import General.Config ( Config(..)
                      , HasAppChan(..)
                      , HasUIUpdaterChan(..)
                      )
import General.Persistence (runDB, PersistentBlockHeader(..), FundRequest(..), PersistentUTXO(..))
import General.Types ( HasNetwork(..)
                     , HasVersion(..)
                     , HasRelay(..)
                     , HasTime(..)
                     , HasLastBlock(..)
                     , HasPeerAddr(..)
                     , Network(..)
                     , HasPool(..))
import General.InternalMessaging (InternalMessage(..), UIUpdaterMessage(..))
import General.Util (Addr(..))

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
import Control.Lens ((^.), (+=), (.~), (%~))
import Control.Monad.Free (Free(..), liftF)

connectTestnet :: Config -> IO () 
connectTestnet config = do
  persistGenesisBlock config
  (context, ioHandlers) <- getConnectionContext config
  forkIO $ listener (ioHandlers^.listenChan) (ioHandlers^.peerSocket)
  forkIO $ writer (ioHandlers^.writerChan) (ioHandlers^.peerSocket)
  connection ioHandlers context
  return ()

getConnectionContext :: Config -> IO (ConnectionContext, IOHandlers)
getConnectionContext config = do
  peer' <- connectToPeer 1 config
  writerChan' <- atomically $ newTBMChan 16
  listenChan' <- atomically $ newTBMChan 16
  time' <- getPOSIXTime
  randGen' <- getStdGen
  lastBlock' <- fromIntegral <$> getLastBlock (config^.pool)
  let connectionContext = ConnectionContext
        { _connectionContextVersion = 60002
        , _connectionContextLastBlock = lastBlock'
        , _myAddr = Addr (0, 0, 0, 0) 18333 
        , _connectionContextPeerAddr = peer'^.addr
        , _connectionContextRelay = False 
        , _connectionContextTime = time'
        , _randGen = randGen'
        , _connectionContextNetwork = config^.network
        }
      ioHandlers = IOHandlers
        { _peerSocket = peer'^.sock
        , _writerChan = writerChan'
        , _listenChan = listenChan'
        , _ioHandlersUIUpdaterChan = config^.uiUpdaterChan
        , _ioHandlersAppChan = config^.appChan
        , _ioHandlersPool = config^.pool}
  return (connectionContext, ioHandlers)
  
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

connection :: IOHandlers -> ConnectionContext -> IO ()
connection ioHandlers context = do
  interpretConnProd ioHandlers context sendVersion'
  interpretConnProd ioHandlers context setFilter'
  connectionLoop ioHandlers context

connectionLoop :: IOHandlers -> ConnectionContext -> IO ()
connectionLoop ioHandlers context = do
  mmResponse <-  liftIO . atomically . tryReadTBMChan $ (ioHandlers^.listenChan)
  case mmResponse of
    Nothing -> fail "listenChan is closed and empty"
    Just Nothing -> return () -- chan is empty
    Just (Just response) -> interpretConnProd ioHandlers context (handleResponse' response)
  mmInternalMessage <- liftIO . atomically . tryReadTBMChan $ (ioHandlers^.appChan)
  case mmInternalMessage of
    Nothing -> fail "appChan is closed and empty"
    Just Nothing -> return () -- chan is empty
    Just (Just internalMessage) ->
      interpretConnProd ioHandlers context (handleInternalMessage' internalMessage)
  connectionLoop ioHandlers context

----------
type KeyId = Integer

data ConnectionInteraction next
  = GetNetwork (Network -> next)
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
  fmap f (GetNetwork                g) = GetNetwork                (f . g)
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
getNetwork' :: Connection' Network
getNetwork' = liftF (GetNetwork id)

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

interpretConnProd :: IOHandlers -> ConnectionContext -> Connection' r -> IO r
interpretConnProd ioHandlers context conn = case conn of
  Free (GetNetwork f) -> do
    interpretConnProd ioHandlers context (f (context^.network))
  Free (GetContext f) -> do
    interpretConnProd ioHandlers context (f context)
  Free (IncrementLastBlock i n) -> do
    let newContext = lastBlock %~ (+ i) $ context
    interpretConnProd ioHandlers newContext n
  Free (ReadMessage f) -> do
    mMessage <- liftIO . atomically . readTBMChan $ (ioHandlers^.listenChan)
    interpretConnProd ioHandlers context (f mMessage)
  Free (WriteMessage m n) -> do
    writeMessage (ioHandlers^.writerChan) m 
    interpretConnProd ioHandlers context n
  Free (WriteUIUpdaterMessage m n) -> do
    writeUiUpdaterMessage (ioHandlers^.uiUpdaterChan) m
    interpretConnProd ioHandlers context n
  Free (GetBlockHeader i f) -> do
    mBlock <- getBlockWithIndex (ioHandlers^.pool) i
    interpretConnProd ioHandlers context (f mBlock)
  Free (BlockHeaderCount f) -> do
    blockHeaderCount <- getLastBlock $ ioHandlers^.pool
    interpretConnProd ioHandlers context (f blockHeaderCount)
  Free (PersistBlockHeaders headers n) -> do
    persistHeaders (ioHandlers^.pool) headers
    interpretConnProd ioHandlers context n
  Free (PersistBlockHeader header n) -> do
    persistHeader (ioHandlers^.pool) header
    interpretConnProd ioHandlers context n
  Free (GetBlockHeaderFromHash hash f) -> do
    mBlock <- getBlockHeaderFromHash (ioHandlers^.pool) hash
    interpretConnProd ioHandlers context (f mBlock)
  Free (NHeadersSinceKey n keyId f) -> do
    headers <- nHeadersSinceKey (ioHandlers^.pool) n keyId
    interpretConnProd ioHandlers context (f headers)
  Free (PersistTransaction tx n) -> do
    persistTransaction (ioHandlers^.pool) tx
    interpretConnProd ioHandlers context n
  Free (GetTransactionFromHash hash f) -> do
    mTx <- getTransactionFromHash (ioHandlers^.pool) hash
    interpretConnProd ioHandlers context (f mTx)
  Free (GetAllAddresses f) -> do
    addresses <- getAllAddresses (ioHandlers^.pool)
    interpretConnProd ioHandlers context (f addresses)
  Free (PersistUTXOs utxos n) -> do
    persistUTXOs (ioHandlers^.pool) utxos
    interpretConnProd ioHandlers context n
  Pure r -> return r

-- Logic for handling response in free monad
handleResponse' :: Message -> Connection' ()
handleResponse' (Message (PingMessageBody message) _) = do
  network' <- getNetwork'
  let pongMessageBody = PongMessageBody . PongMessage $ message^.nonce64
      pongMessage =
        Message pongMessageBody (MessageContext network')
  writeMessage' pongMessage

handleResponse' (Message (VersionMessageBody body) _) = do
  network' <- getNetwork'
  let verackMessage =
        Message (VerackMessageBody VerackMessage) (MessageContext (network'))
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
  network' <- getNetwork'
  match <- firstHeaderMatch' (message^.blockLocatorHashes)
  matchingHeaders <- 2000 `nHeadersSinceKey'` match
  let headersMessage =
        Message
         (HeadersMessageBody (HeadersMessage {_blockHeaders = matchingHeaders }))
         (MessageContext (network'))
  writeMessage' headersMessage

handleResponse' (Message (InvMessageBody message) _) = do
  network' <- getNetwork'
  desiredInvs <- map toFilteredBlock <$> filterM (desiredData) (message^.invVectors)
  let getDataMessage =
        Message
        (GetDataMessageBody (GetDataMessage desiredInvs))
        (MessageContext (network'))
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
  network' <- getNetwork'
  let getHeadersMessage' lastBlock' = do
        blockLocatorHashes' <- queryBlockLocatorHashes' lastBlock'
        return $ Message
          (bodyConstructor (messageConstructor (context^.version) blockLocatorHashes'
           (BlockHash . fst . decode $
            "0000000000000000000000000000000000000000000000000000000000000000")))
          (MessageContext (network'))
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
  network' <- getNetwork'
  let nonce' = Nonce64 . fst $ randomR (0, 0xffffffffffffffff) (context^.randGen)
               -- TODO: We need to update the randGen after calling randomR
      versionMessage = Message
        (VersionMessageBody (VersionMessage
          (context^.version)
          nonce'
          (context^.lastBlock)
          (context^.peerAddr)
          (context^.myAddr)
          (context^.relay)
          (context^.time)))
        (MessageContext (network'))
  writeMessage' versionMessage

setFilter' :: Connection' ()
setFilter' = do
  network' <- getNetwork'
  let getPubKeyHashBS (PubKeyHash bs) = bs
  pubKeyHashes <- map (getPubKeyHashBS . addressToPubKeyHash) <$> getAllAddresses'
  let
    (filter', filterContext') = defaultFilterWithElements pubKeyHashes
    filterloadMessage = Message
      (FilterloadMessageBody (FilterloadMessage filter' filterContext' BLOOM_UPDATE_NONE))
      (MessageContext (network'))
  writeMessage' filterloadMessage

handleInternalMessage' :: InternalMessage -> Connection' ()
handleInternalMessage' (SendTX transaction') = do
  network' <- getNetwork'
  let body = TxMessageBody . TxMessage $ transaction'
      messageContext = MessageContext (network')
      txMessage = Message body messageContext
  writeMessage' txMessage

handleInternalMessage' (AddAddress address) = do
  network' <- getNetwork'
  let getPubKeyHashBS (PubKeyHash bs) = bs
      body = FilteraddMessageBody
           . FilteraddMessage
           . getPubKeyHashBS
           . addressToPubKeyHash
           $ address
      messageContext = MessageContext (network')
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
