{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
module Protocol.Server where

import Protocol.Messages
  ( parseMessage
  , Message(..)
  , MessageBody(..)
  , MessageContext(..)
  )
import Protocol.MessageBodies 
import Protocol.Network (connectToPeer, sock, addr)
import Protocol.Util (getUTXOS, HasLastBlock(..), BlockIndex(..))
import qualified Protocol.Persistence as Persistence
import Protocol.ConnectionM
  ( ConnectionContext(..)
  , IOHandlers(..)
  , MutableConnectionContext(..)
  , InterpreterContext(..)
  , LogEntry(..)
  , LogLevel(..)
  , logLevel
  , logStr
  , ioHandlers
  , logFilter
  , context
  , peerSocket
  , myAddr
  , writerChan
  , listenChan
  , randGen
  , rejectedBlocks
  , mutableContext
  , displayLogs
  )
import BitcoinCore.BlockHeaders
  ( BlockHash(..)
  , BlockHeader(..)
  , verifyHeaders
  , prevBlockHash
  , hashBlock
  , showBlocks
  )
import BitcoinCore.BloomFilter
  ( NFlags(..)
  , defaultFilterWithElements
  )
import BitcoinCore.Keys (addressToPubKeyHash, Address(..))
import BitcoinCore.Inventory (InventoryVector(..), ObjectType(..), objectHash)
import BitcoinCore.Transaction.Transactions
  ( Value(..)
  , Transaction(..)
  , TxHash
  , hashTransaction
  )
import BitcoinCore.MerkleTrees (MerkleHash(..))
import General.Config
  ( Config(..)
  , HasAppChan(..)
  , HasUIUpdaterChan(..)
  )
import General.Persistence (PersistentUTXO(..))
import General.Types
  ( HasNetwork(..)
  , HasVersion(..)
  , HasRelay(..)
  , HasTime(..)
  , HasPeerAddr(..)
  , HasPool(..)
  )
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
import Data.Conduit.TMChan
  ( sourceTBMChan
  , sinkTBMChan
  , newTBMChan
  , TBMChan
  , writeTBMChan
  , readTBMChan
  , tryReadTBMChan
  )
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Data.Binary (Binary(..))
import Data.ByteString.Base16 (decode)
import qualified Data.ByteString as BS
import Control.Lens ((^.), (%~), (.~))
import Control.Monad.Free (Free(..), liftF, iterM)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import qualified Database.Persist as DB
import Control.Monad.Trans.State (StateT(..), execStateT)
import qualified Control.Monad.Trans.State as ST

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
      logAll _ = True
      ic = InterpreterContext
        { _ioHandlers = ioHandlers'
        , _context = connectionContext
        , _logFilter = logAll }
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
  newIC <- execStateT (interpretConnProd sendVersion') ic
  newIC' <- execStateT (interpretConnProd setFilter') newIC
  connectionLoop newIC'
  
connectionLoop :: InterpreterContext -> IO ()
connectionLoop ic = do
  let readChan = atomically . tryReadTBMChan
  mmResponse <-  readChan $ (ic^.ioHandlers.listenChan)
  newIC <- case mmResponse of
    Nothing -> fail "listenChan is closed and empty"
    Just Nothing -> return ic -- chan is empty
    Just (Just response) -> execStateT (interpretConnProd (handleResponse' response)) ic
  mmInternalMessage <- readChan $ (newIC^.ioHandlers.appChan)
  newIC' <- case mmInternalMessage of
    Nothing -> fail "appChan is closed and empty"
    Just Nothing -> return newIC -- chan is empty
    Just (Just internalMessage) ->
      execStateT (interpretConnProd (handleInternalMessage' internalMessage)) newIC
  connectionLoop newIC'

data ContextEff next =
    GetContext (ConnectionContext -> next)
  | SetContext MutableConnectionContext next
    deriving Functor

data NetworkEff next =
    ReadMessage (Maybe Message -> next)
  | WriteMessage Message next
  | WriteUIUpdaterMessage UIUpdaterMessage next
    deriving Functor

data DataEff next =
    GetBlockHeader BlockIndex (Maybe BlockHeader -> next)
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
  | GetUnspentUTXOs ([DB.Entity PersistentUTXO] -> next)
  | SetUTXOBlockHash (DB.Key PersistentUTXO) BlockHash next
    deriving Functor

data LogEff next = WriteLog LogEntry next
  deriving Functor

data ConnectionInteraction next =
    Data (DataEff next)
  | Net (NetworkEff next)
  | Context (ContextEff next)
  | Log (LogEff next)
    deriving Functor

type Connection' = Free ConnectionInteraction

-- syntactic sugar to enable `do` notation
getContext' :: Connection' ConnectionContext
getContext' = liftF (Context $ GetContext id)

setContext' :: MutableConnectionContext -> Connection' ()
setContext' mc = liftF (Context $ SetContext mc ())

readMessage' :: Connection' (Maybe Message)
readMessage' = liftF (Net $ ReadMessage id)

writeMessage' :: Message -> Connection' ()
writeMessage' message = liftF (Net $ WriteMessage message ())

writeUiUpdaterMessage' :: UIUpdaterMessage -> Connection' ()
writeUiUpdaterMessage' message = liftF (Net $ WriteUIUpdaterMessage message ())

getBlockHeader' :: BlockIndex -> Connection' (Maybe BlockHeader)
getBlockHeader' index = liftF (Data $ GetBlockHeader index id)

blockHeaderCount' :: Connection' BlockIndex
blockHeaderCount' = liftF (Data $ BlockHeaderCount id)

persistHeaders' :: [BlockHeader] -> Connection' ()
persistHeaders' headers = do
  contextOld <- getContext'
  liftF (Data $ PersistBlockHeaders headers ())
  incrementLastBlock' $ length headers
  contextNew <- getContext'
  logDebug' $
    "Adding blocks to main chain: " ++ showBlocks headers
    ++ "\n\t`lastBlock` changed from "
    ++ show (contextOld^.mutableContext.lastBlock)
    ++ " to " ++ show (contextNew^.mutableContext.lastBlock)

persistHeader' :: BlockHeader -> Connection' ()
persistHeader' header = do
  contextOld <- getContext'
  liftF (Data $ PersistBlockHeader header ())
  incrementLastBlock' 1
  contextNew <- getContext'
  logDebug' $
    "Adding single block to main chain: " ++ showBlocks [header]
    ++ "\n\t`lastBlock` changed from "
    ++ show (contextOld^.mutableContext.lastBlock)
    ++ " to " ++ show (contextNew^.mutableContext.lastBlock)

getBlockHeaderFromHash' :: BlockHash
                        -> Connection' (Maybe (BlockIndex, BlockHeader))
getBlockHeaderFromHash' hash = liftF (Data $ GetBlockHeaderFromHash hash id)

deleteBlockHeaders' :: BlockIndex -> Connection' ()
deleteBlockHeaders' inx = do
  contextOld <- getContext'
  liftF (Data $ DeleteBlockHeaders inx ())
  setLastBlock' (inx - 1)
  contextNew <- getContext'
  logDebug' $
    "Deleting all blocks all blocks with index >= " ++ show inx
    ++ "\n\t`lastBlock` changed from "
    ++ show (contextOld^.mutableContext.lastBlock)
    ++ " to " ++ show (contextNew^.mutableContext.lastBlock)

persistTransaction' :: Transaction -> Connection' ()
persistTransaction' tx = liftF (Data $ PersistTransaction tx ())

nHeadersSinceKey' :: Int -> BlockIndex -> Connection' [BlockHeader]
nHeadersSinceKey' n keyId = liftF (Data $ NHeadersSinceKey n keyId id)

getTransactionFromHash' :: TxHash -> Connection' (Maybe Integer)
getTransactionFromHash' hash = liftF (Data $ GetTransactionFromHash hash id)

getAllAddresses' :: Connection' [Address]
getAllAddresses' = liftF (Data $ GetAllAddresses id)

persistUTXOs' :: [PersistentUTXO] -> Connection' ()
persistUTXOs' persistentUTXOs = liftF (Data $ PersistUTXOs persistentUTXOs ())

getUnspentUTXOs' :: Connection' [DB.Entity PersistentUTXO]
getUnspentUTXOs' = liftF (Data $ GetUnspentUTXOs id)

setUTXOBlockHash' :: (DB.Key PersistentUTXO) -> BlockHash -> Connection' ()
setUTXOBlockHash' k h = liftF (Data $ SetUTXOBlockHash k h ())

log' :: LogEntry -> Connection' ()
log' le = liftF (Log $ WriteLog le ())

incrementLastBlock' :: Int -> Connection' ()
incrementLastBlock' i = do
  context <- getContext'
  let newContext = mutableContext.lastBlock
                   %~ (\(BlockIndex bi) -> (BlockIndex (bi + i)))
                   $ context
  setContext' $ newContext^.mutableContext
  
setLastBlock' :: BlockIndex -> Connection' ()
setLastBlock' i = do
  context <- getContext'
  let newContext = mutableContext.lastBlock .~ i $ context
  setContext' $ newContext^.mutableContext

execContext :: ContextEff (StateT InterpreterContext IO r) -> StateT InterpreterContext IO r
execContext c = do
  ic <- ST.get
  case c of
    GetContext f -> f $ ic^.context
    SetContext mc n -> do
      ST.put $ context.mutableContext .~ mc $ ic
      n

execNet :: NetworkEff (StateT InterpreterContext IO r) -> StateT InterpreterContext IO r
execNet c = do
  ic <- ST.get
  case c of
    ReadMessage f -> do
      mMessage <- liftIO . atomically . readTBMChan $ (ic^.ioHandlers.listenChan)
      f mMessage
    WriteMessage m n -> do
      liftIO $ writeMessage (ic^.ioHandlers.writerChan) m 
      n
    WriteUIUpdaterMessage m n -> do
      liftIO $ writeUiUpdaterMessage (ic^.ioHandlers.uiUpdaterChan) m
      n

execData :: DataEff (StateT InterpreterContext IO r) -> StateT InterpreterContext IO r
execData conn = do
  ic <- ST.get
  case conn of
    GetBlockHeader i f -> do
      mBlock <- liftIO $ Persistence.getBlockWithIndex (ic^.ioHandlers.pool) i
      f mBlock
    BlockHeaderCount f -> do
      blockHeaderCount <- liftIO $ Persistence.getLastBlock $ ic^.ioHandlers.pool
      f blockHeaderCount
    PersistBlockHeaders headers n -> do
      liftIO $ Persistence.persistHeaders (ic^.ioHandlers.pool) headers
      n
    PersistBlockHeader header n -> do
      liftIO $ Persistence.persistHeader (ic^.ioHandlers.pool) header
      n
    GetBlockHeaderFromHash hash f -> do
      mBlock <- liftIO $ Persistence.getBlockHeaderFromHash (ic^.ioHandlers.pool) hash
      f mBlock
    DeleteBlockHeaders inx n -> do
      liftIO $ Persistence.deleteHeaders (ic^.ioHandlers.pool) inx
      n
    NHeadersSinceKey n keyId f -> do
      headers <- liftIO $ Persistence.nHeadersSinceKey (ic^.ioHandlers.pool) n keyId
      f headers
    PersistTransaction tx n -> do
      liftIO $ Persistence.persistTransaction (ic^.ioHandlers.pool) tx
      n
    GetTransactionFromHash hash f -> do
      mTx <- liftIO $ Persistence.getTransactionFromHash (ic^.ioHandlers.pool) hash
      f mTx
    GetAllAddresses f -> do
      addresses <- liftIO $ Persistence.getAllAddresses (ic^.ioHandlers.pool)
      f addresses
    PersistUTXOs utxos n -> do
      liftIO $ Persistence.persistUTXOs (ic^.ioHandlers.pool) utxos
      n
    GetUnspentUTXOs f -> do
      utxos <- liftIO $ Persistence.getUnspentUTXOs (ic^.ioHandlers.pool)
      f utxos
    SetUTXOBlockHash k h n -> do
      liftIO $ Persistence.setUTXOBlockHash (ic^.ioHandlers.pool) k h
      n

execLog :: LogEff (StateT InterpreterContext IO r) -> StateT InterpreterContext IO r
execLog conn = do
  ic <- ST.get
  case conn of
    WriteLog le n -> do
      liftIO $ putStrLn $ displayLogs (ic^.logFilter) [le]
      n

execConnProd :: ConnectionInteraction (StateT InterpreterContext IO r) -> StateT InterpreterContext IO r
execConnProd (Data d) = execData d
execConnProd (Net n) = execNet n
execConnProd (Context c) = execContext c
execConnProd (Log l) = execLog l

interpretConnProd :: Connection' r -> StateT InterpreterContext IO r
interpretConnProd = iterM execConnProd

-- Logic for handling response in free monad
handleResponse' :: Message -> Connection' ()
handleResponse' (Message (PingMessageBody message) _) = 
  writeMessageWithBody' . PongMessageBody $ PongMessage (message^.nonce64)

handleResponse' (Message (VersionMessageBody body) _) = do
  writeMessageWithBody' . VerackMessageBody $ VerackMessage
  let lastBlockPeer = body^.lastBlock
  synchronizeHeaders' lastBlockPeer

handleResponse' (Message (HeadersMessageBody (HeadersMessage headers)) _) = do
  handleNewHeaders headers
  getDataMerkleBlock headers -- request merkle blocks to look for our utxos

handleResponse' (Message (MerkleblockMessageBody message) _) = do
  handleNewHeaders [message^.blockHeader]
  updateUTXOs message -- update utxos with this block header in the db

handleResponse' (Message (GetHeadersMessageBody message) _) = do
  match <- firstHeaderMatch' (message^.blockLocatorHashes)
  matchingHeaders <- 2000 `nHeadersSinceKey'` match
  writeMessageWithBody'
    . HeadersMessageBody
    $ HeadersMessage {_blockHeaders = matchingHeaders }
 
handleResponse' (Message (InvMessageBody message) _) = do
  let desiredData = message^.invVectors
      desiredInvs = map toFilteredBlock desiredData
  writeMessageWithBody' . GetDataMessageBody $ GetDataMessage desiredInvs
  where
    toFilteredBlock (InventoryVector MSG_BLOCK hash) = InventoryVector
                                                       MSG_FILTERED_BLOCK
                                                       hash
    toFilteredBlock invVector = invVector

-- TODO: If this is not atomic, then there is a race condition
--       two threads may see isHandled False
--       and then proceed to persist duplicate UTXOs
handleResponse' (Message (TxMessageBody message) _) = do
  isHandled <- isTransactionHandled'
  unless isHandled $ do
    logDebug' $ "Transaction was not previously handled. Handling now."
    persistentUTXOs <- retrieveUTXOs -- utxos to us in this tx
    persistUTXOs' persistentUTXOs
    let val = sum $ map (persistentUTXOValue) persistentUTXOs
    if val > 0 then writeUiUpdaterMessage' -- notify frontend of new utxo
                  . IncomingFunds
                  . Satoshis
                  $ val
               else return ()
    persistTransaction' (message^.transaction)
  where
    retrieveUTXOs = do
      pubKeyHashes <- map (hash . addressToPubKeyHash) <$> getAllAddresses'
      let indexedPubkeyHashes = zip [1..] pubKeyHashes
          persistentUTXOs = getUTXOS indexedPubkeyHashes (message^.transaction)
      return persistentUTXOs
    isTransactionHandled' = do
      let txHash = hashTransaction $ message^.transaction
      mTx <- getTransactionFromHash' txHash
      return $ case mTx of
        Nothing -> False
        Just _  -> True

handleResponse' message = logError' $
  "We are not yet able to handle message" ++ show message

-- | If any of our utxos are in the Merkle Block, update their blockHash in the
-- DB.
updateUTXOs :: MerkleblockMessage -> Connection' ()
updateUTXOs merkleBlock = do
  let getHash (DB.Entity _ x) = MerkleHash
                              . BS.reverse
                              . persistentUTXOOutTxHash $ x
    -- ^ note that the hash seems to be reversed
  utxos <- getUnspentUTXOs'
  let hashes = merkleBlock^.merkleHashes
      block = merkleBlock^.blockHeader
      utxosInBlock =
        filter (\x -> getHash x `elem` hashes) utxos
  if length utxosInBlock > 0
    then do
      logDebug' $ "found a block for utxos " ++
        (intercalate " " $ map (show . DB.entityKey) utxosInBlock)
      mapM_ (\x -> setUTXOBlockHash' (DB.entityKey x) (hashBlock block))
        utxosInBlock
        -- ^ set the block hash for each utxo in this block
      writeUiUpdaterMessage' UTXOsUpdated
    else return ()

getDataMerkleBlock :: [BlockHeader] -> Connection' ()
getDataMerkleBlock bs = do
  let inventoryObj = map ( InventoryVector MSG_FILTERED_BLOCK
                         . objectHash
                         . hash
                         . hashBlock) bs
      body = GetDataMessageBody $ GetDataMessage inventoryObj
  writeMessageWithBody' body

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
  writeUiUpdaterMessage' NewBlock

-- Try to construct a new blockchain using `headers`
-- if we can construct a new blockchain and it's longer than the active chain,
-- then we replace the active chain. Otherwise,
-- we store `headers` in rejectedBlocks
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
        "Constructing chain. Found block connecting to main chain at: "
        ++ show index
      replaceChainIfLonger header index
  where
    constructChainFromRejectedBlocks connectingBlockHash = do
      context <- getContext'
      let rejectedBlocks' = context^.mutableContext.rejectedBlocks
          blockMatches header = hashBlock header == connectingBlockHash
          newConnectingBlockHash = filter blockMatches rejectedBlocks'
      case newConnectingBlockHash of 
        [] -> addRejected headers
          -- we can't add any blocks from `rejectedBlocks`
          -- to our candidate chain
        (x:xs) -> constructChain (x:headers)
          -- we can add `x` from `rejectedBlocks`
          -- to our candidate chain and try again

    addRejected headers = do
      prevContext <- getContext' 
      setContext' $ rejectedBlocks %~ (++ headers) $ prevContext^.mutableContext

    replaceChainIfLonger connectionHeader connectionIndex = do
      context <- getContext'
      let newLength = connectionIndex + BlockIndex (length headers)
          headersAreValid = verifyHeaders (connectionHeader:headers)
      logDebug' $ "Checking if we should replace the chain."
        ++ "\n\tActiveChain length: "
        ++ show (context^.mutableContext.lastBlock)
        ++ "\n\tNewChain length: " ++ show newLength
        ++ "\n\tNew chain is valid? " ++ show headersAreValid
      if newLength > context^.mutableContext.lastBlock
         && headersAreValid
        then replaceChain headers connectionIndex
        else addRejected headers
      
    replaceChain newChain connectionIndex = do
      logDebug' "Replacing main chain"
      context <- getContext'
      let inxs = enumFromTo
                 (connectionIndex + 1)
                 (context^.mutableContext.lastBlock)
      newRejected <- mapM getBlockHeader' inxs
      -- TODO: also remove inxs headers from rejected
      addRejected $ catMaybes newRejected
      deleteBlockHeaders' (connectionIndex + 1)
      persistHeaders' newChain


-- returns the leftmost header that we are currently persisting
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
    Nothing -> fail
      "Unable to get most recent block header. This should never happen"
    Just lastBlockHeader -> return lastBlockHeader

synchronizeHeaders' :: BlockIndex -> Connection' ()
synchronizeHeaders' lastBlockPeer = do
  logDebug' "Synchronizing headers"
  context <- getContext'
  newSync <- isNewSync'
  when (outOfSync' context) $ do
    logDebug' "We are out of sync with our peer"
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

    -- Handle all incoming messages recursively until we recieve more headers.
    -- When we recieve more headers, we can continue synchronizing.
    handleMessages' = do
      mResponse <- readMessage'
      case mResponse of
        Nothing -> fail "unable to read message"
        Just response@(Message (HeadersMessageBody _) _) ->
          handleResponse' response
        Just response@(Message (MerkleblockMessageBody _) _) ->
          handleResponse' response
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
  let lastBlock' = context^.mutableContext.lastBlock
  blockLocatorHashes' <- queryBlockLocatorHashes' lastBlock'
  let allBlocksHashStop = Hash . fst . decode $
        "0000000000000000000000000000000000000000000000000000000000000000"
      body = bodyConstructor $ messageConstructor
             (context^.version)
             blockLocatorHashes'
             allBlocksHashStop
      message = Message body (MessageContext (context^.network))
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
  let nonce' = Nonce64 . fst $ randomR (0, maxNonce) randGen'
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
      maxNonce = 0xffffffffffffffff -- 8 bytes
      randGen' = context^.mutableContext.randGen
  writeMessage' versionMessage

setFilter' :: Connection' ()
setFilter' = do
  context <- getContext'
  pubKeyHashes <- map (hash . addressToPubKeyHash) <$> getAllAddresses'
  let
    (filter', filterContext') = defaultFilterWithElements pubKeyHashes
    filterloadMessage = Message
      (FilterloadMessageBody
        (FilterloadMessage filter' filterContext' BLOOM_UPDATE_NONE))
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
blockLocatorIndices lastBlock' = reverse
                                 . addGenesisIndiceIfNeeded
                                 $ blockLocatorIndicesStep 10 1 [lastBlock']
  where addGenesisIndiceIfNeeded indices@((BlockIndex 0):xs) = indices
        addGenesisIndiceIfNeeded xs   = (BlockIndex 0):xs

blockLocatorIndicesStep :: Int -> Int -> [BlockIndex] -> [BlockIndex]
blockLocatorIndicesStep c step indices@((BlockIndex i):is)
  | c > 0 && i > 0 = blockLocatorIndicesStep
                     (c - 1)
                     step
                     ((BlockIndex $ i - 1):indices)
  | i - step > 0 = blockLocatorIndicesStep
                   c
                   (step * 2)
                   ((BlockIndex $ i - step):indices)
  | otherwise = indices
blockLocatorIndicesStep _ _ [] = error
  "blockLocatorIndicesStep must be called with a nonempty accummulator array"

writeMessage :: TBMChan Message -> Message -> IO ()
writeMessage chan message = 
  atomically $ writeTBMChan chan message

writeUiUpdaterMessage :: TBMChan UIUpdaterMessage -> UIUpdaterMessage -> IO ()
writeUiUpdaterMessage chan message =
  atomically $ writeTBMChan chan message

logDebug' :: String -> Connection' ()
logDebug' = log' . LogEntry Debug

logError' :: String -> Connection' ()
logError' = log' . LogEntry Error
