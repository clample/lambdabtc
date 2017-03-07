{-# LANGUAGE TemplateHaskell #-}
module ProtocolServerTest where

import TestUtil
import Protocol.Messages ( Message(..)
                         , MessageBody(..)
                         , MessageContext(..)
                         , arbitraryPingMessage)
import Protocol.Server
import Protocol.ConnectionM ( ConnectionContext(..))
import General.InternalMessaging ( UIUpdaterMessage(..)
                                 , InternalMessage(..))
import BitcoinCore.Transaction.Transactions (TxHash(..), hashTransaction)
import BitcoinCore.BlockHeaders (BlockHeader(..), hashBlock)
import BitcoinCore.Keys (Address(..))
import General.Types (HasLastBlock(..), Network(..))
import General.Persistence (PersistentUTXO(..))
import General.Util (Addr(..), IP(..), Port(..))

import Control.Lens (makeLenses, (^.), (%~), (.~))
import Control.Monad.Free (Free(..))
import Data.List (findIndex)
import Control.Monad.Identity (Identity(..), runIdentity)
import System.Random (mkStdGen)
import Test.QuickCheck.Gen (generate)

newtype PingMessage = PingMessage Message
  deriving (Show, Eq)

instance Arbitrary PingMessage where
  arbitrary = PingMessage <$>
    (Message <$> arbitraryPingMessage <*> arbitrary)

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
  Free (IncrementLastBlock i n) -> do
    let newContext = lastBlock %~ (+ i) $ context
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
    let headers = mockHandles^.mockDB.blockHeaders
        i' = (fromIntegral i) - 1
    if ((length headers) < i')
      then
        interpretConnTest mockHandles context (f Nothing)
      else do
        let blockHeader = Just $ headers !! i'
        interpretConnTest mockHandles context (f blockHeader)
  Free (BlockHeaderCount f) -> do
    let blockHeaderCount = (length $ mockHandles^.mockDB.blockHeaders) - 1
    interpretConnTest mockHandles context (f blockHeaderCount)
  Free (PersistBlockHeaders headers n) -> do
    let newMockHandles = mockDB.blockHeaders %~ (++ headers) $ mockHandles
    interpretConnTest newMockHandles context n
  Free (PersistBlockHeader header n) -> do
    let newMockHandles = mockDB.blockHeaders %~ (++ [header]) $ mockHandles
    interpretConnTest newMockHandles context n
  Free (GetBlockHeaderFromHash hash f) -> do
    let headers = mockHandles^.mockDB.blockHeaders
        mIndex = findIndex (\header -> hashBlock header == hash) headers
        mBlockHeader = case mIndex of
          Nothing -> Nothing
          Just i -> Just (1 + fromIntegral i, headers !! i)
    interpretConnTest mockHandles context (f mBlockHeader)
  Free (NHeadersSinceKey n keyId f) -> do
    let keyId' = (fromIntegral keyId) - 1
        headers = mockHandles^.mockDB.blockHeaders
        nHeaders = (drop keyId' . take n) $ headers
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

pingAndPong = testCase
  "We should respond to a single ping message with a single pong message"
  $ do
      pingMessage <- generate arbitrary
      (assertBool "PingPong test" $ prop_pingAndPong pingMessage)

prop_pingAndPong :: PingMessage -> Bool
prop_pingAndPong (PingMessage message) =
  case result^.outgoingMessageList of
    [Message (PongMessageBody messageBody) _] -> True
    _ -> False
  where
    (result, ()) = runIdentity $
      interpretConnTest blankMockHandles genericConnectionContext (handleResponse' message)
    

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
  , _connectionContextLastBlock = 0
  , _myAddr = Addr (0, 0, 0, 0) 80
  , _connectionContextPeerAddr = Addr (0, 0, 0, 1) 80
  , _connectionContextRelay = True
  , _connectionContextTime = fromInteger 10000
  , _randGen = mkStdGen 1
  , _connectionContextNetwork = TestNet3
  }
