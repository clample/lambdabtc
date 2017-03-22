{-# LANGUAGE OverloadedStrings #-}
module Protocol.Server where

import Protocol.Messages (parseMessage, Message(..), MessageBody(..), MessageContext(..))
import Protocol.MessageBodies 
import Protocol.Network (connectToPeer, sock, addr)
import Protocol.Util (getUTXOS, HasLastBlock(..), BlockIndex(..))
import qualified Protocol.Persistence as Persistence
import Protocol.ConnectionM ( ConnectionContext(..)
                            , IOHandlers(..)
                            , MutableConnectionContext(..)
                            , InterpreterContext(..)
                            , LogEntry(..)
                            , LogLevel(..)
                            , logLevel
                            , logStr
                            , ioHandlers
                            , logs
                            , context
                            , peerSocket
                            , myAddr
                            , writerChan
                            , listenChan
                            , randGen
                            , rejectedBlocks
                            , mutableContext)
import BitcoinCore.BlockHeaders ( BlockHash(..)
                                , BlockHeader(..)
                                , verifyHeaders
                                , prevBlockHash
                                , hashBlock
                                , showBlocks
                                )
import BitcoinCore.BloomFilter ( NFlags(..)
                               , defaultFilterWithElements)
import BitcoinCore.Keys (addressToPubKeyHash, Address(..))
import BitcoinCore.Inventory (InventoryVector(..), ObjectType(..))
import BitcoinCore.Transaction.Transactions ( Value(..)
                                            , Transaction(..)
                                            , TxHash
                                            , hashTransaction
                                            )
import General.Config ( Config(..)
                      , HasAppChan(..)
                      , HasUIUpdaterChan(..)
                      )
import General.Persistence (PersistentUTXO(..))
import General.Types ( HasNetwork(..)
                     , HasVersion(..)
                     , HasRelay(..)
                     , HasTime(..)
                     , HasPeerAddr(..)
                     , HasPool(..))
import General.InternalMessaging (InternalMessage(..), UIUpdaterMessage(..))
import General.Util (Addr(..))
import General.Hash (Hash(..))

import Network.Socket (Socket)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.State.Lazy (liftIO)
import Control.Monad (when, filterM, unless)
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
import Control.Lens ((^.), (%~), (.~))
import Control.Monad.Free (Free(..), liftF)
import Data.Maybe (catMaybes)

connectTestnet :: Config -> IO () 
connectTestnet config = do
  Persistence.persistGenesisBlock config
  ic <- getConnectionContext config
  forkIO $ listener (ic^.ioHandlers.listenChan) (ic^.ioHandlers.peerSocket)
  forkIO $ writer (ic^.ioHandlers.writerChan) (ic^.ioHandlers.peerSocket)
  connection ic
  return ()

getConnectionContext :: Config -> IO InterpreterContext
getConnectionContext config = do
  peer' <- connectToPeer 1 config
  writerChan' <- atomically $ newTBMChan 16
  listenChan' <- atomically $ newTBMChan 16
  time' <- getPOSIXTime
  randGen' <- getStdGen
  lastBlock' <- Persistence.getLastBlock (config^.pool)
  let connectionContext = ConnectionContext
        { _connectionContextVersion = 60002
        , _myAddr = Addr (0, 0, 0, 0) 18333 
        , _connectionContextPeerAddr = peer'^.addr
        , _connectionContextRelay = False 
        , _connectionContextTime = time'
        , _connectionContextNetwork = config^.network
        , _mutableContext = mutableContext'
        }
      mutableContext' = MutableConnectionContext
        { _randGen = randGen'
        , _rejectedBlocks = []
        , _connectionContextLastBlock = lastBlock'
        }
      ioHandlers' = IOHandlers
        { _peerSocket = peer'^.sock
        , _writerChan = writerChan'
        , _listenChan = listenChan'
        , _ioHandlersUIUpdaterChan = config^.uiUpdaterChan
        , _ioHandlersAppChan = config^.appChan
        , _ioHandlersPool = config^.pool}
      ic = InterpreterContext
        { _ioHandlers = ioHandlers'
        , _context = connectionContext
        , _logs = [] }
  return ic
  
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

connection :: InterpreterContext -> IO ()
connection ic = do
  interpretConnProd ic sendVersion'
  interpretConnProd ic setFilter'
  connectionLoop ic
  
connectionLoop :: InterpreterContext -> IO ()
connectionLoop ic = do
  mmResponse <-  liftIO . atomically . tryReadTBMChan $ (ic^.ioHandlers.listenChan)
  case mmResponse of
    Nothing -> fail "listenChan is closed and empty"
    Just Nothing -> return () -- chan is empty
    Just (Just response) -> interpretConnProd ic (handleResponse' response)
  mmInternalMessage <- liftIO . atomically . tryReadTBMChan $ (ic^.ioHandlers.appChan)
  case mmInternalMessage of
    Nothing -> fail "appChan is closed and empty"
    Just Nothing -> return () -- chan is empty
    Just (Just internalMessage) ->
      interpretConnProd ic (handleInternalMessage' internalMessage)
  connectionLoop ic

data ConnectionInteraction next
  = GetContext (ConnectionContext -> next)
  | SetContext MutableConnectionContext next
  | ReadMessage (Maybe Message -> next)
  | WriteMessage Message next
  | WriteUIUpdaterMessage UIUpdaterMessage next
  | GetBlockHeader BlockIndex (Maybe BlockHeader -> next)
  | BlockHeaderCount (BlockIndex -> next)
  | PersistBlockHeaders [BlockHeader] next
  | PersistBlockHeader BlockHeader next
  | GetBlockHeaderFromHash BlockHash (Maybe (BlockIndex, BlockHeader) -> next)
  | DeleteBlockHeaders BlockIndex next
  | PersistTransaction Transaction next
  | NHeadersSinceKey Int BlockIndex ([BlockHeader] -> next)
  | GetTransactionFromHash TxHash (Maybe Integer -> next)
  | GetAllAddresses ([Address] -> next)
  | PersistUTXOs [PersistentUTXO] next
  | Log LogEntry next

instance Functor ConnectionInteraction where
  fmap f (GetContext                g) = GetContext                (f . g)
  fmap f (SetContext              c x) = SetContext              c (f x)
  fmap f (ReadMessage               g) = ReadMessage               (f . g)
  fmap f (WriteMessage            m x) = WriteMessage            m (f x)
  fmap f (WriteUIUpdaterMessage   m x) = WriteUIUpdaterMessage   m (f x)
  fmap f (GetBlockHeader          i g) = GetBlockHeader          i (f . g)
  fmap f (BlockHeaderCount          g) = BlockHeaderCount          (f . g)
  fmap f (PersistBlockHeaders     h x) = PersistBlockHeaders     h (f x)
  fmap f (PersistBlockHeader      h x) = PersistBlockHeader      h (f x)
  fmap f (GetBlockHeaderFromHash  h g) = GetBlockHeaderFromHash  h (f . g)
  fmap f (DeleteBlockHeaders      i x) = DeleteBlockHeaders      i (f x)
  fmap f (PersistTransaction      t x) = PersistTransaction      t (f x)
  fmap f (NHeadersSinceKey        n i g) = NHeadersSinceKey    n i (f . g)
  fmap f (GetTransactionFromHash  h g) = GetTransactionFromHash  h (f . g)
  fmap f (GetAllAddresses           g) = GetAllAddresses           (f . g)
  fmap f (PersistUTXOs            u x) = PersistUTXOs            u (f x)
  fmap f (Log                    le x) = Log                    le (f x)

type Connection' = Free ConnectionInteraction

-- syntactic sugar to enable `do` notation
getContext' :: Connection' ConnectionContext
getContext' = liftF (GetContext id)

setContext' :: MutableConnectionContext -> Connection' ()
setContext' mc = liftF (SetContext mc ())

readMessage' :: Connection' (Maybe Message)
readMessage' = liftF (ReadMessage id)

writeMessage' :: Message -> Connection' ()
writeMessage' message = liftF (WriteMessage message ())

writeUiUpdaterMessage' :: UIUpdaterMessage -> Connection' ()
writeUiUpdaterMessage' message = liftF (WriteUIUpdaterMessage message ())

getBlockHeader' :: BlockIndex -> Connection' (Maybe BlockHeader)
getBlockHeader' index = liftF (GetBlockHeader index id)

blockHeaderCount' :: Connection' BlockIndex
blockHeaderCount' = liftF (BlockHeaderCount id)

persistHeaders' :: [BlockHeader] -> Connection' ()
persistHeaders' headers = do
  contextOld <- getContext'
  liftF (PersistBlockHeaders headers ())
  incrementLastBlock' $ length headers
  contextNew <- getContext'
  logDebug' $
    "Adding blocks to main chain: " ++ showBlocks headers
    ++ "\n\t`lastBlock` changed from "
    ++ show (contextOld^.mutableContext.lastBlock)
    ++ " to " ++ show (contextNew^.mutableContext.lastBlock)
  

persistHeader' :: BlockHeader -> Connection' ()
persistHeader' header = do
  liftF (PersistBlockHeader header ())
  incrementLastBlock' 1

getBlockHeaderFromHash' :: BlockHash -> Connection' (Maybe (BlockIndex, BlockHeader))
getBlockHeaderFromHash' hash = liftF (GetBlockHeaderFromHash hash id)

deleteBlockHeaders' :: BlockIndex -> Connection' ()
deleteBlockHeaders' inx = do
  contextOld <- getContext'
  liftF (DeleteBlockHeaders inx ())
  setLastBlock' (inx - 1)
  contextNew <- getContext'
  logDebug' $
    "Deleting all blocks all blocks with index >= " ++ show inx
    ++ "\n\t`lastBlock` changed from "
    ++ show (contextOld^.mutableContext.lastBlock)
    ++ " to " ++ show (contextNew^.mutableContext.lastBlock)

persistTransaction' :: Transaction -> Connection' ()
persistTransaction' tx = liftF (PersistTransaction tx ())

nHeadersSinceKey' :: Int -> BlockIndex -> Connection' [BlockHeader]
nHeadersSinceKey' n keyId = liftF (NHeadersSinceKey n keyId id)

getTransactionFromHash' :: TxHash -> Connection' (Maybe Integer)
getTransactionFromHash' hash = liftF (GetTransactionFromHash hash id)

getAllAddresses' :: Connection' [Address]
getAllAddresses' = liftF (GetAllAddresses id)

persistUTXOs' :: [PersistentUTXO] -> Connection' ()
persistUTXOs' persistentUTXOs = liftF (PersistUTXOs persistentUTXOs ())

log' :: LogEntry -> Connection' ()
log' le = liftF (Log le ())

incrementLastBlock' :: Int -> Connection' ()
incrementLastBlock' i = do
  context <- getContext'
  let newContext = mutableContext.lastBlock %~ (\(BlockIndex bi) -> (BlockIndex (bi + i)))
                   $ context
  setContext' $ newContext^.mutableContext
  
setLastBlock' :: BlockIndex -> Connection' ()
setLastBlock' i = do
  context <- getContext'
  let newContext = mutableContext.lastBlock .~ i $ context
  setContext' $ newContext^.mutableContext

interpretConnProd :: InterpreterContext -> Connection' r -> IO r
interpretConnProd ic conn = case conn of
  Free (GetContext f) -> 
    interpretConnProd ic (f $ ic^.context)
  Free (SetContext mc n) -> do
    let newIC = context.mutableContext .~ mc $ ic
    interpretConnProd newIC n
  Free (ReadMessage f) -> do
    mMessage <- liftIO . atomically . readTBMChan $ (ic^.ioHandlers.listenChan)
    interpretConnProd ic (f mMessage)
  Free (WriteMessage m n) -> do
    writeMessage (ic^.ioHandlers.writerChan) m 
    interpretConnProd ic n
  Free (WriteUIUpdaterMessage m n) -> do
    writeUiUpdaterMessage (ic^.ioHandlers.uiUpdaterChan) m
    interpretConnProd ic n
  Free (GetBlockHeader i f) -> do
    mBlock <- Persistence.getBlockWithIndex (ic^.ioHandlers.pool) i
    interpretConnProd ic (f mBlock)
  Free (BlockHeaderCount f) -> do
    blockHeaderCount <- Persistence.getLastBlock $ ic^.ioHandlers.pool
    interpretConnProd ic (f blockHeaderCount)
  Free (PersistBlockHeaders headers n) -> do
    Persistence.persistHeaders (ic^.ioHandlers.pool) headers
    interpretConnProd ic n
  Free (PersistBlockHeader header n) -> do
    Persistence.persistHeader (ic^.ioHandlers.pool) header
    interpretConnProd ic n
  Free (GetBlockHeaderFromHash hash f) -> do
    mBlock <- Persistence.getBlockHeaderFromHash (ic^.ioHandlers.pool) hash
    interpretConnProd ic (f mBlock)
  Free (DeleteBlockHeaders inx n) -> do
    Persistence.deleteHeaders (ic^.ioHandlers.pool) inx
    interpretConnProd ic n
  Free (NHeadersSinceKey n keyId f) -> do
    headers <- Persistence.nHeadersSinceKey (ic^.ioHandlers.pool) n keyId
    interpretConnProd ic (f headers)
  Free (PersistTransaction tx n) -> do
    Persistence.persistTransaction (ic^.ioHandlers.pool) tx
    interpretConnProd ic n
  Free (GetTransactionFromHash hash f) -> do
    mTx <- Persistence.getTransactionFromHash (ic^.ioHandlers.pool) hash
    interpretConnProd ic (f mTx)
  Free (GetAllAddresses f) -> do
    addresses <- Persistence.getAllAddresses (ic^.ioHandlers.pool)
    interpretConnProd ic (f addresses)
  Free (PersistUTXOs utxos n) -> do
    Persistence.persistUTXOs (ic^.ioHandlers.pool) utxos
    interpretConnProd ic n
  Free (Log le n) -> do
    let newIC = logs %~ (++ [le]) $ ic
    interpretConnProd newIC n
  Pure r -> return r

-- Logic for handling response in free monad
handleResponse' :: Message -> Connection' ()
handleResponse' (Message (PingMessageBody message) _) = 
  writeMessageWithBody' . PongMessageBody $ PongMessage (message^.nonce64)

handleResponse' (Message (VersionMessageBody body) _) = do
  writeMessageWithBody' . VerackMessageBody $ VerackMessage
  let lastBlockPeer = body^.lastBlock
  synchronizeHeaders' lastBlockPeer

handleResponse' (Message (HeadersMessageBody (HeadersMessage headers)) _) =
  handleNewHeaders headers

handleResponse' (Message (MerkleblockMessageBody message) _) =
  handleNewHeaders [message^.blockHeader]

handleResponse' (Message (GetHeadersMessageBody message) _) = do
  match <- firstHeaderMatch' (message^.blockLocatorHashes)
  matchingHeaders <- 2000 `nHeadersSinceKey'` match
  writeMessageWithBody' . HeadersMessageBody $ HeadersMessage {_blockHeaders = matchingHeaders }
 
handleResponse' (Message (InvMessageBody message) _) = do
  desiredInvs <- map toFilteredBlock <$> filterM desiredData (message^.invVectors)
  writeMessageWithBody' . GetDataMessageBody $ GetDataMessage desiredInvs
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
  unless isHandled $ do
    addUTXOs
    writeUiUpdaterMessage' . IncomingFunds . Satoshis $ 1000
    persistTransaction' (message^.transaction)
  where
    addUTXOs = do
      pubKeyHashes <- map (hash . addressToPubKeyHash) <$> getAllAddresses'
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
  "We are not yet able to handle message" ++ show message

writeMessageWithBody' :: MessageBody -> Connection' ()
writeMessageWithBody' body = do
  context <- getContext'
  let message = Message body (MessageContext (context^.network))
  writeMessage' message

handleNewHeaders :: [BlockHeader] -> Connection' ()
handleNewHeaders newHeaders = do
  mostRecentHeader <- getMostRecentHeader'
  let isValid = verifyHeaders (mostRecentHeader:newHeaders)
  if isValid
    then persistHeaders' newHeaders
    else constructChain newHeaders

-- Try to construct a new blockchain using `headers`
-- if we can construct a new blockchain and it's longer than the active chain,
-- then we replace the active chain. Otherwise, we store `headers` in rejectedBlocks
-- in case we are able to construct a longer chain later on.
constructChain :: [BlockHeader] -> Connection' ()
constructChain headers = do  
  let connectingBlockHash = (head headers)^.prevBlockHash
  logDebug' $ "Constructing Chain with headers "
    ++ showBlocks headers
  connectingBlock <- getBlockHeaderFromHash' connectingBlockHash
  case connectingBlock of
    
    -- `headers` does not connect to the active chain
    Nothing -> constructChainFromRejectedBlocks connectingBlockHash
    
    -- `headers` does connect to the active chain
    Just (index, header) -> do
      logDebug' $
        "Constructing chain. Found block connecting to main chain at: " ++ show index
      replaceChainIfLonger header index
  where
    constructChainFromRejectedBlocks connectingBlockHash = do
      context <- getContext'
      let rejectedBlocks' = context^.mutableContext.rejectedBlocks
          newConnectingBlockHash = filter (\header -> hashBlock header == connectingBlockHash) rejectedBlocks'
      case newConnectingBlockHash of 
        [] -> addRejected headers
          -- we can't add any blocks from `rejectedBlocks` to our candidate chain
        (x:xs) -> constructChain (x:headers)
          -- we can add `x` from `rejectedBlocks` to our candidate chain and try again

    addRejected headers = do
      prevContext <- getContext' 
      setContext' $ rejectedBlocks %~ (++ headers) $ prevContext^.mutableContext

    replaceChainIfLonger connectionHeader connectionIndex = do
      context <- getContext'
      let newLength = connectionIndex + BlockIndex (length headers)
          headersAreValid = verifyHeaders (connectionHeader:headers)
      logDebug' $ "Checking if we should replace the chain."
        ++ "\n\tActiveChain length: " ++ show (context^.mutableContext.lastBlock)
        ++ "\n\tNewChain length: " ++ show newLength
        ++ "\n\tNew chain is valid? " ++ show headersAreValid
      if newLength > context^.mutableContext.lastBlock
         && headersAreValid
        then replaceChain headers connectionIndex
        else addRejected headers
      
    replaceChain newChain connectionIndex = do
      logDebug' "Replacing main chain"
      context <- getContext'
      let inxs = enumFromTo (connectionIndex + 1) (context^.mutableContext.lastBlock)
      newRejected <- mapM getBlockHeader' inxs
      -- TODO: also remove inxs headers from rejected
      addRejected $ catMaybes newRejected
      deleteBlockHeaders' (connectionIndex + 1)
      persistHeaders' newChain


-- returns the leftmose header that we are currently persisting
firstHeaderMatch' :: [BlockHash] -> Connection' BlockIndex
firstHeaderMatch' [] = fail "No matching hash was found"
firstHeaderMatch' (hash:hashes) = do
  mHeader <- getBlockHeaderFromHash' hash
  case mHeader of
    Just (index, _) -> return index
    Nothing -> firstHeaderMatch' hashes

getMostRecentHeader' :: Connection' BlockHeader
getMostRecentHeader' = do
  blockHeaderCount <- blockHeaderCount'
  mLastBlockHeader <- getBlockHeader' blockHeaderCount
  case mLastBlockHeader of
    Nothing -> fail "Unable to get most recent block header. This should never happen"
    Just lastBlockHeader -> return lastBlockHeader

synchronizeHeaders' :: BlockIndex -> Connection' ()
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
    outOfSync' context = context^.mutableContext.lastBlock < lastBlockPeer
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
  let getHeadersMessage' lastBlock' = do
        blockLocatorHashes' <- queryBlockLocatorHashes' lastBlock'
        return $ Message
          (bodyConstructor (messageConstructor (context^.version) blockLocatorHashes'
           (Hash . fst . decode $
            "0000000000000000000000000000000000000000000000000000000000000000")))
          (MessageContext (context^.network))
  message <- getHeadersMessage' (context^.mutableContext.lastBlock)
  writeMessage' message

-- returns a list of some of the block hashes for headers we've persisted
-- this is used by the peer to send us headers starting with where
-- we diverge from their main chain
queryBlockLocatorHashes' :: BlockIndex -> Connection' [BlockHash]
queryBlockLocatorHashes' lastBlock' =
  catMaybes <$> mapM getMBlockHash (blockLocatorIndices lastBlock')
  where
    getMBlockHash i = do
      header <- getBlockHeader' i
      return $ hashBlock <$> header

isNewSync' :: Connection' Bool
isNewSync' = (== []) <$> getAllAddresses'
  -- When there are no addresses
  -- we can assume that no transactions have been sent to us

sendVersion' :: Connection' ()
sendVersion' = do
  context <- getContext'
  let nonce' = Nonce64 . fst $ randomR (0, 0xffffffffffffffff) (context^.mutableContext.randGen)
               -- TODO: We need to update the randGen after calling randomR
      versionMessage = Message
        (VersionMessageBody (VersionMessage
          (context^.version)
          nonce'
          (context^.mutableContext.lastBlock)
          (context^.peerAddr)
          (context^.myAddr)
          (context^.relay)
          (context^.time)))
        (MessageContext (context^.network))
  writeMessage' versionMessage

setFilter' :: Connection' ()
setFilter' = do
  context <- getContext'
  pubKeyHashes <- map (hash . addressToPubKeyHash) <$> getAllAddresses'
  let
    (filter', filterContext') = defaultFilterWithElements pubKeyHashes
    filterloadMessage = Message
      (FilterloadMessageBody (FilterloadMessage filter' filterContext' BLOOM_UPDATE_NONE))
      (MessageContext (context^.network))
  writeMessage' filterloadMessage

handleInternalMessage' :: InternalMessage -> Connection' ()
handleInternalMessage' (SendTX transaction') = do
  context <- getContext'
  let body = TxMessageBody . TxMessage $ transaction'
      messageContext = MessageContext (context^.network)
      txMessage = Message body messageContext
  writeMessage' txMessage

handleInternalMessage' (AddAddress address) = do
  context <- getContext'
  let body = FilteraddMessageBody
           . FilteraddMessage
           . hash
           . addressToPubKeyHash
           $ address
      messageContext = MessageContext (context^.network)
      filterAddMessage = Message body messageContext
  writeMessage' filterAddMessage

blockLocatorIndices :: BlockIndex -> [BlockIndex]
blockLocatorIndices lastBlock' = reverse . addGenesisIndiceIfNeeded $ blockLocatorIndicesStep 10 1 [lastBlock']
  where addGenesisIndiceIfNeeded indices@((BlockIndex 0):xs) = indices
        addGenesisIndiceIfNeeded xs   = (BlockIndex 0):xs

blockLocatorIndicesStep :: Int -> Int -> [BlockIndex] -> [BlockIndex]
blockLocatorIndicesStep c step indices@((BlockIndex i):is)
  | c > 0 && i > 0 = blockLocatorIndicesStep (c - 1) step ((BlockIndex $ i - 1):indices)
  | i - step > 0 = blockLocatorIndicesStep c (step * 2) ((BlockIndex $ i - step):indices)
  | otherwise = indices
blockLocatorIndicesStep _ _ [] =
  error "blockLocatorIndicesStep must be called with a nonempty accummulator array "

writeMessage :: TBMChan Message -> Message -> IO ()
writeMessage chan message = 
  liftIO . atomically $ writeTBMChan chan message

writeUiUpdaterMessage :: TBMChan UIUpdaterMessage -> UIUpdaterMessage -> IO ()
writeUiUpdaterMessage chan message =
  liftIO . atomically $ writeTBMChan chan message

logDebug' :: String -> Connection' ()
logDebug' = log' . LogEntry Debug
