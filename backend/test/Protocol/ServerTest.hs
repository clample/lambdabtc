{-# LANGUAGE TemplateHaskell #-}
module Protocol.ServerTest where

import TestUtil
import Protocol.Messages ( Message(..)
                         , MessageBody(..)
                         , MessageContext(..)
                         , arbitraryPingMessage
                         , arbitraryVersionMessage)
import Protocol.MessageBodies ( HeadersMessage( HeadersMessage))
import Protocol.Server
import Protocol.ConnectionM ( ConnectionContext(..)
                            , MutableConnectionContext(..)
                            , mutableContext
                            , LogEntry
                            , displayLogs
                            , LogLevel(..))
import General.InternalMessaging ( UIUpdaterMessage(..)
                                 , InternalMessage(..))
import BitcoinCore.Transaction.Transactions (TxHash, hashTransaction)
import BitcoinCore.BlockHeaders ( BlockHeader(..)
                                , hashBlock
                                , ValidHeaders(..)
                                , ValidBlockTree(..)
                                , verifyHeaders
                                , showBlocks)
import BitcoinCore.Keys (Address(..))
import General.Types (Network(..))
import General.Persistence (PersistentUTXO(..))
import General.Util ( Addr(..)
                    , IP(..)
                    , Port(..)
                    , getBranch
                    , Tree(..)
                    , node
                    , subTree
                    , branches)
import Protocol.Util (HasLastBlock(..), BlockIndex(..))

import Control.Lens (makeLenses, (^.), (%~), (.~))
import Control.Monad.Free (Free(..))
import Data.List (findIndex, any, maximumBy)
import Data.Function (on)
import Control.Monad.Identity (Identity(..), runIdentity)
import System.Random (mkStdGen)
import Test.QuickCheck.Gen (generate)
import Test.QuickCheck (ioProperty, Property)
import Test.QuickCheck.Property (counterexample)

newtype ArbPingMessage = ArbPingMessage Message
  deriving (Show, Eq)

instance Arbitrary ArbPingMessage where
  arbitrary = ArbPingMessage <$>
    (Message <$> arbitraryPingMessage <*> arbitrary)

newtype ArbVersionMessage = ArbVersionMessage Message
  deriving (Show, Eq)

instance Arbitrary ArbVersionMessage where
  arbitrary = ArbVersionMessage <$>
    (Message <$> arbitraryVersionMessage <*> arbitrary)

data MockHandles = MockHandles
  { _incomingMessageList :: [Message]
  , _outgoingMessageList :: [Message]
  , _outgoingUIUpdaterMessages :: [UIUpdaterMessage]
  , _incomingAppChanMessages :: [InternalMessage]
  , _mockDB :: MockDB
  }

data MockDB = MockDB
  { _blockHeaders :: [BlockHeader]
  , _transactions :: [TxHash]
  , _addresses    :: [Address]
  , _utxos        :: [PersistentUTXO]
  }

data TestInterpreterContext = TestInterpreterContext
  { _context  :: ConnectionContext
  , _handlers :: MockHandles
  , _logs :: [LogEntry]
  }

makeLenses ''MockHandles
makeLenses ''MockDB
makeLenses ''TestInterpreterContext

interpretConnTest :: TestInterpreterContext
                  -> Connection' r
                  -> Identity (TestInterpreterContext, r)
interpretConnTest ic conn =  case conn of
  Free (GetContext f) -> do
    interpretConnTest ic (f $ ic^.context)
  Free (SetContext mc n) -> do
    let newIC = context.mutableContext .~ mc $ ic
    interpretConnTest newIC n 
  Free (ReadMessage f) -> do
    case ic^.handlers.incomingMessageList of
      [] ->
        interpretConnTest ic (f Nothing)
      m:ms -> do
        let newIC = handlers.incomingMessageList .~ ms $ ic
        interpretConnTest newIC (f (Just m))
  Free (WriteMessage m n) -> do
    let newIC = handlers.outgoingMessageList %~ (m:) $ ic
    interpretConnTest newIC n
  Free (WriteUIUpdaterMessage m n) -> do
    let newIC = handlers.outgoingUIUpdaterMessages %~ (m:) $ ic
    interpretConnTest newIC n
  Free (GetBlockHeader i f) -> do
    if (blockHeaderCount < i)
      then
        interpretConnTest ic (f Nothing)
      else do
        let (BlockIndex i') = i
            blockHeader = Just $ (ic^.handlers.mockDB.blockHeaders) !! i'
        interpretConnTest ic (f blockHeader)
  Free (BlockHeaderCount f) -> do
    interpretConnTest ic (f blockHeaderCount)
  Free (PersistBlockHeaders headers n) -> do
    let newIC = handlers.mockDB.blockHeaders %~ (++ headers)
          $ incrementLastBlock ic (length headers)
    interpretConnTest newIC n
  Free (PersistBlockHeader header n) -> do
    let newIC = handlers.mockDB.blockHeaders %~ (++ [header])
          $ incrementLastBlock ic 1
    interpretConnTest newIC n
  Free (GetBlockHeaderFromHash hash f) -> do
    let headers = ic^.handlers.mockDB.blockHeaders
        mIndex = findIndex (\header -> hashBlock header == hash) headers
        mBlockHeader = case mIndex of
          Nothing -> Nothing
          Just i -> Just (BlockIndex i, headers !! i)
    interpretConnTest ic (f mBlockHeader)
  Free (DeleteBlockHeaders bi@(BlockIndex inx) n) -> do
    let ic'  = handlers.mockDB.blockHeaders %~ (take inx) $ ic
        ic'' = context.lastBlock .~ (bi - 1) $ ic'
    interpretConnTest ic'' n
  Free (NHeadersSinceKey n (BlockIndex i) f) -> do
    let headers = ic^.handlers.mockDB.blockHeaders
        nHeaders = (drop i . take n) $ headers
    interpretConnTest ic (f nHeaders)
  Free (PersistTransaction tx n) -> do
    let newIC = handlers.mockDB.transactions %~ (++ [hashTransaction tx]) $ ic
    interpretConnTest newIC n
  Free (GetTransactionFromHash hash f) -> do
    let txs = ic^.handlers.mockDB.transactions
        mIndex = (fromIntegral . (1 +)) <$> findIndex (== hash) txs
    interpretConnTest ic (f mIndex)
  Free (GetAllAddresses f) -> do
    let allAddresses = ic^.handlers.mockDB.addresses
    interpretConnTest ic (f allAddresses)
  Free (PersistUTXOs newUtxos n) -> do
    let newIC = handlers.mockDB.utxos %~ (++ newUtxos) $ ic
    interpretConnTest newIC n
  Free (Log le n) -> do
    let newIC = logs %~ (++ [le]) $ ic
    interpretConnTest newIC n
  Pure r -> return (ic, r)
  where
    blockHeaderCount = BlockIndex $ (length $ ic^.handlers.mockDB.blockHeaders) - 1
    incrementLastBlock ::  TestInterpreterContext -> Int -> TestInterpreterContext
    incrementLastBlock c i =
      context.lastBlock %~ (\(BlockIndex old) -> BlockIndex (old + i)) $ c

pingAndPong = testCase
  "We should respond to a single ping message with a single pong message"
  $ do
      pingMessage <- generate arbitrary
      (assertBool "PingPong test" $ prop_pingAndPong pingMessage)

prop_pingAndPong :: ArbPingMessage -> Bool
prop_pingAndPong (ArbPingMessage message) =
  case result^.handlers.outgoingMessageList of
    [Message (PongMessageBody messageBody) _] -> True
    _ -> False
  where
    (result, ()) = runIdentity $
      interpretConnTest defaultIC (handleResponse' message)

-- Currently, handleResponse' VersionMessage causes us to call synchronizeHeaders
-- this test case currently doesn't handle this
versionAndVerack = testCase
  "We should respond to a version message with a verack message"
  $ do
      versionMessage <- generate arbitrary
      (assertBool "VersionVerack test" $ prop_versionAndVerack versionMessage)

prop_versionAndVerack :: ArbVersionMessage -> Bool
prop_versionAndVerack (ArbVersionMessage message) =
  any isVerack (result^.handlers.outgoingMessageList)
  where
    (result, ()) = runIdentity $
      interpretConnTest defaultIC (handleResponse' message)
    isVerack message = case message of
      (Message (VerackMessageBody _) _) -> True
      _                                  -> False

longerChain = testProperty
  "Should switch to longer chain" prop_longerChain

prop_longerChain :: ValidHeaders -> MessageContext -> Bool
prop_longerChain (ValidHeaders (h1:h2:hs)) msgContext = 
  resultHandle^.handlers.mockDB.blockHeaders == (h1:h2:hs)   
  where
    ic = (handlers.mockDB.blockHeaders .~ [h1,h2]) 
               $ defaultIC
    headersMessage = (Message (HeadersMessageBody (HeadersMessage (h2:hs))) msgContext)
    (resultHandle, _) = runIdentity
                        $ interpretConnTest ic (handleResponse' headersMessage)

longerChain' = testProperty
  "We should use the correct active chain"
  prop_longerChain'

prop_longerChain' :: ValidBlockTree -> MessageContext -> Property
prop_longerChain' (ValidBlockTree tree) msgContext = counterexample
  debugString
  (length activeChain == length longestBranch)
  where
    ic = (handlers.mockDB.blockHeaders .~ [genesisBlock]) defaultIC
    genesisBlock = tree^.node
    toHeadersMessage hs = (Message (HeadersMessageBody (HeadersMessage hs)) msgContext)
    branches' = filter (/= []) . map (drop 1) . branches $ tree
    longestBranch = maximumBy (compare `on` length) . branches $ tree
    activeChain = resultHandle^.handlers.mockDB.blockHeaders
    (resultHandle, _) = runIdentity $ interpretConnTest ic $
      mapM (handleResponse' . toHeadersMessage) branches'
    logAll _ = True
    debugString = unlines
      [ displayLogs logAll $ resultHandle^.logs
      , "Active chain: " ++ showBlocks activeChain
      , "Actual longest chain: " ++ showBlocks longestBranch
      ]

defaultIC :: TestInterpreterContext
defaultIC = TestInterpreterContext
  { _context = genericConnectionContext
  , _handlers = blankMockHandles
  , _logs = []
  }

blankMockHandles :: MockHandles
blankMockHandles = MockHandles
  { _incomingMessageList = []
  , _outgoingMessageList = []
  , _outgoingUIUpdaterMessages = []
  , _incomingAppChanMessages = []
  , _mockDB = mockDB'
  }
  where mockDB' = MockDB
          { _blockHeaders = []
          , _transactions = []
          , _addresses = []
          , _utxos = []}

genericConnectionContext :: ConnectionContext
genericConnectionContext = ConnectionContext
  { _connectionContextVersion = 100
  , _connectionContextLastBlock = BlockIndex 0
  , _myAddr = Addr (0, 0, 0, 0) 80
  , _connectionContextPeerAddr = Addr (0, 0, 0, 1) 80
  , _connectionContextRelay = True
  , _connectionContextTime = fromInteger 10000
  , _connectionContextNetwork = TestNet3
  , _mutableContext = MutableConnectionContext
    { _randGen = mkStdGen 1
    , _rejectedBlocks = []
    }
  }
