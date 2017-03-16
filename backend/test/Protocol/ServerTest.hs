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
                            , mutableContext)
import General.InternalMessaging ( UIUpdaterMessage(..)
                                 , InternalMessage(..))
import BitcoinCore.Transaction.Transactions (TxHash, hashTransaction)
import BitcoinCore.BlockHeaders (BlockHeader(..), hashBlock, ValidHeaders(..))
import BitcoinCore.Keys (Address(..))
import General.Types (Network(..))
import General.Persistence (PersistentUTXO(..))
import General.Util (Addr(..), IP(..), Port(..))
import Protocol.Util (HasLastBlock(..), BlockIndex(..))

import Control.Lens (makeLenses, (^.), (%~), (.~))
import Control.Monad.Free (Free(..))
import Data.List (findIndex, any)
import Control.Monad.Identity (Identity(..), runIdentity)
import System.Random (mkStdGen)
import Test.QuickCheck.Gen (generate)

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

makeLenses ''MockHandles
makeLenses ''MockDB

interpretConnTest :: MockHandles -> ConnectionContext -> Connection' r -> Identity (MockHandles, r)
interpretConnTest mockHandles context conn =  case conn of
  Free (GetContext f) -> do
    interpretConnTest mockHandles context (f context)
  Free (SetContext mc n) -> do
    let newContext = mutableContext .~ mc $ context
    interpretConnTest mockHandles newContext n 
  Free (ReadMessage f) -> do
    case mockHandles^.incomingMessageList of
      [] ->
        interpretConnTest mockHandles context (f Nothing)
      m:ms -> do
        let newMockHandles = incomingMessageList .~ ms $ mockHandles
        interpretConnTest newMockHandles context (f (Just m))
  Free (WriteMessage m n) -> do
    let newMockHandles = outgoingMessageList %~ (m:) $ mockHandles
    interpretConnTest newMockHandles context n
  Free (WriteUIUpdaterMessage m n) -> do
    let newMockHandles = outgoingUIUpdaterMessages %~ (m:) $ mockHandles
    interpretConnTest newMockHandles context n
  Free (GetBlockHeader i f) -> do
    if (blockHeaderCount < i)
      then
        interpretConnTest mockHandles context (f Nothing)
      else do
        let (BlockIndex i') = i
            blockHeader = Just $ (mockHandles^.mockDB.blockHeaders) !! i'
        interpretConnTest mockHandles context (f blockHeader)
  Free (BlockHeaderCount f) -> do
    interpretConnTest mockHandles context (f blockHeaderCount)
  Free (PersistBlockHeaders headers n) -> do
    let newMockHandles = mockDB.blockHeaders %~ (++ headers) $ mockHandles
        newContext = incrementLastBlock context (length headers)
    interpretConnTest newMockHandles newContext n
  Free (PersistBlockHeader header n) -> do
    let newMockHandles = mockDB.blockHeaders %~ (++ [header]) $ mockHandles
        newContext = incrementLastBlock context 1
    interpretConnTest newMockHandles newContext n
  Free (GetBlockHeaderFromHash hash f) -> do
    let headers = mockHandles^.mockDB.blockHeaders
        mIndex = findIndex (\header -> hashBlock header == hash) headers
        mBlockHeader = case mIndex of
          Nothing -> Nothing
          Just i -> Just (BlockIndex i, headers !! i)
    interpretConnTest mockHandles context (f mBlockHeader)
  Free (DeleteBlockHeaders (BlockIndex inx) n) -> do
    let newMockHandles = mockDB.blockHeaders %~ (take inx) $ mockHandles
        newContext = lastBlock .~ blockHeaderCount $ context
    interpretConnTest newMockHandles context n
  Free (NHeadersSinceKey n (BlockIndex i) f) -> do
    let headers = mockHandles^.mockDB.blockHeaders
        nHeaders = (drop i . take n) $ headers
    interpretConnTest mockHandles context (f nHeaders)
  Free (PersistTransaction tx n) -> do
    let newMockHandles = mockDB.transactions %~ (++ [hashTransaction tx]) $ mockHandles
    interpretConnTest newMockHandles context n
  Free (GetTransactionFromHash hash f) -> do
    let txs = mockHandles^.mockDB.transactions
        mIndex = (fromIntegral . (1 +)) <$> findIndex (== hash) txs
    interpretConnTest mockHandles context (f mIndex)
  Free (GetAllAddresses f) -> do
    let allAddresses = mockHandles^.mockDB.addresses
    interpretConnTest mockHandles context (f allAddresses)
  Free (PersistUTXOs newUtxos n) -> do
    let newMockHandles = mockDB.utxos %~ (++ newUtxos) $ mockHandles
    interpretConnTest newMockHandles context n
  Pure r -> return (mockHandles, r)
  where blockHeaderCount = BlockIndex $ (length $ mockHandles^.mockDB.blockHeaders) - 1

pingAndPong = testCase
  "We should respond to a single ping message with a single pong message"
  $ do
      pingMessage <- generate arbitrary
      (assertBool "PingPong test" $ prop_pingAndPong pingMessage)

prop_pingAndPong :: ArbPingMessage -> Bool
prop_pingAndPong (ArbPingMessage message) =
  case result^.outgoingMessageList of
    [Message (PongMessageBody messageBody) _] -> True
    _ -> False
  where
    (result, ()) = runIdentity $
      interpretConnTest blankMockHandles genericConnectionContext (handleResponse' message)

-- Currently, handleResponse' VersionMessage causes us to call synchronizeHeaders
-- this test case currently doesn't handle this
versionAndVerack = testCase
  "We should respond to a version message with a verack message"
  $ do
      versionMessage <- generate arbitrary
      (assertBool "VersionVerack test" $ prop_versionAndVerack versionMessage)

prop_versionAndVerack :: ArbVersionMessage -> Bool
prop_versionAndVerack (ArbVersionMessage message) =
  any isVerack (result^.outgoingMessageList)
  where
    (result, ()) = runIdentity $
      interpretConnTest blankMockHandles genericConnectionContext (handleResponse' message)
    isVerack message = case message of
      (Message (VerackMessageBody _) _) -> True
      _                                  -> False

longerChain = testProperty
  "Should switch to longer chain" prop_longerChain

prop_longerChain :: ValidHeaders -> MessageContext -> Bool
prop_longerChain (ValidHeaders (h1:h2:hs)) msgContext = 
  resultHandle^.mockDB.blockHeaders == (h1:h2:hs)   
  where
    mockHandle = (mockDB . blockHeaders .~ [h1,h2]) 
               $ blankMockHandles
    headersMessage = (Message (HeadersMessageBody (HeadersMessage (h2:hs))) msgContext)
    (resultHandle, _) = runIdentity $ interpretConnTest mockHandle 
        genericConnectionContext (handleResponse' headersMessage)


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
