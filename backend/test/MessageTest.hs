module MessageTest where

import TestUtil
import Protocol.Messages (putMessage)
import Protocol.Types (Command(..), commandTable, Network(..), networkTable, Header(..), Addr(..), MessageBody(..), MessageContext(..), Message(..))
import Protocol.Parser (parseMessage)
import qualified Data.ByteString.Char8 as Char8
import Data.Time.Clock (NominalDiffTime(..))
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Base16 (encode, decode)
import BitcoinCore.BlockHeaders
import BitcoinCore.BloomFilter
import BitcoinCore.Inventory

instance Arbitrary Message where
  arbitrary = Message <$> arbitrary <*> arbitrary

instance Arbitrary MessageBody where
  arbitrary = oneof
    [ arbitraryVersionMessage
    , arbitraryGetHeadersMessage
    , arbitraryGetHeadersMessage
    , arbitraryFilterloadMessage
    , arbitraryInvMessage
    , return VerackMessage ]

instance Arbitrary MessageContext where
  arbitrary = MessageContext <$> arbitrary


arbitraryVersionMessage = do
  version    <- choose (0, maxVersion)
  nonceInt   <- choose (0, maxNonce) :: Gen Integer
  lastBlockN <- choose (0, maxBlock)
  senderAddr <- arbitrary
  peerAddr   <- arbitrary
  relay      <- arbitrary
  time       <- choose (0, maxTime) :: Gen Integer
  return $ VersionMessage version nonceInt lastBlockN senderAddr peerAddr relay (realToFrac time)
  where
    maxVersion = 0xffffffff         -- 4 bytes
    maxNonce   = 0xffffffffffffffff -- 8 bytes
    maxBlock   = 0xffffffff         -- 4 bytes
    maxTime = 0xffffffffffffffff -- 8 bytes

arbitraryGetHeadersMessage = do
  version <- choose (0, maxVersion)
  n      <- choose (0, 2000)
  blockLocatorHashes <- vectorOf n arbitrary
  hashStop <- arbitrary
  return $ GetHeadersMessage version blockLocatorHashes hashStop
  where maxVersion = 0xffffffff -- 4 bytes

arbitraryHeadersMessage = do
  n            <- choose (0, 2000) -- A headers message contains at most 2000 block headers
  blockHeaders <- vectorOf n arbitrary
  return $ HeadersMessage blockHeaders

arbitraryFilterloadMessage = do
  fValue <- choose (0, 0xffffffffffffffff) -- upper limit is so the value is reasonably sized
  let minEncodingLength = B.length . unroll $ fValue
  fLengthBytes <- choose (minEncodingLength, 2 * minEncodingLength)
  let filter = Filter { filterLengthBytes = fLengthBytes, filterValue = fValue}
  nHashFuncs <- choose (0, maxNHashFuncs)
  nTweak <- Tweak <$> choose (0, maxNTweak)
  nFlags <- arbitraryBoundedEnum
  return $ FilterloadMessage  filter nHashFuncs nTweak nFlags
  where
    maxNHashFuncs = 0xffffffff -- 4 bytes
    maxNTweak     = 0xffffffff -- 4 bytes

arbitraryInvMessage = 
  InvMessage <$> arbitrary
  
instance Arbitrary InventoryVector where
  arbitrary = do
    objType <- arbitraryBoundedEnum
    objHash <- ObjectHash . Char8.pack <$> vector 32
    return $ InventoryVector objType objHash

  
instance Arbitrary Network where
  arbitrary = do
    let networks = map fst networkTable
    elements networks

instance Arbitrary Addr where
  arbitrary = do
    a <- chooseIpComponent
    b <- chooseIpComponent
    c <- chooseIpComponent
    d <- chooseIpComponent
    port <- choosePort
    return $ Addr (a, b, c, d) port
    where
      chooseIpComponent = choose (0, 255)
      choosePort = choose (0, 65535)

messageInvertible = testProperty
  "It should be possible to encode and decode messages"
  prop_messageInvertible

prop_messageInvertible :: Message -> Bool
prop_messageInvertible message =
      parsedMessage == message
  where
    parsedMessage = runGet parseMessage (runPut . putMessage $ message)
    
