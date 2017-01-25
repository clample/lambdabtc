module MessageTest where

import TestUtil
import Messages (Command(..), commandTable, Network(..), networkTable, Header(..), showHeader, Addr(..), networkAddress, VersionMessage(..), showVersionMessage)
import Protocol.Parser (parseHeader, parseAddr, parseVersionMessage)
import Text.Megaparsec (parseMaybe)
import qualified Data.ByteString.Char8 as Char8
import Data.Time.Clock (NominalDiffTime(..))

instance Arbitrary Command where
  arbitrary = do
    let commands = map fst commandTable
    elements commands

instance Arbitrary Network where
  arbitrary = do
    let networks = map fst networkTable
    elements networks

instance Arbitrary Header where
  arbitrary = do
    command <- arbitrary
    network <- arbitrary
    messageLength <- arbitrary
    message <- hexBS messageLength
    return $ Header network command message

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

instance Arbitrary VersionMessage where
  arbitrary = do
    version    <- choose (0, maxVersion) 
    network    <- arbitrary
    time       <- choose (0, maxTime)  :: Gen Integer
    nonceInt   <- choose (0, maxNonce) :: Gen Integer
    lastBlockN <- choose (0, maxBlock)
    senderAddr <- arbitrary
    peerAddr   <- arbitrary
    return $ VersionMessage version network (realToFrac time) nonceInt lastBlockN senderAddr peerAddr
    where
      maxVersion = 0xffffffff         -- 4 bytes
      maxTime    = 0xffffffffffffffff -- 8 bytes
      maxNonce   = 0xffffffffffffffff -- 8 bytes
      maxBlock   = 0xffffffff         -- 4 bytes

messageHeaderInvertible = testProperty
  "It should be possible to encode and parse a message header"
  prop_messageHeaderInvertible

prop_messageHeaderInvertible :: Header -> Bool
prop_messageHeaderInvertible header@(Header network command _) =
  case eitherHeader of
    Nothing -> False
    Just (Header network' command' _) ->
      network == network' &&
      command == command'
  where
    eitherHeader = parseMaybe parseHeader headerString
    headerString = Char8.unpack . showHeader $ header

addrInvertible = testProperty
  "It should be possible to encode and decode an Addr"
  prop_addrInvertible

prop_addrInvertible :: Addr -> Bool
prop_addrInvertible addr =
  case eitherAddr of
    Nothing -> False
    Just parsedAddr -> parsedAddr == addr
  where
    eitherAddr = parseMaybe parseAddr addrString
    addrString = Char8.unpack $ networkAddress addr

versionMessageInvertible = testProperty
  "It should be possible to encode and decode a versionMessage"
  prop_versionMessageInvertible

prop_versionMessageInvertible :: VersionMessage -> Bool
prop_versionMessageInvertible versionMsg =
  case eitherVersionMsg of
    Nothing -> False
    Just parsedVersionMsg ->
      parsedVersionMsg == versionMsg
  where
    eitherVersionMsg = parseMaybe parseVersionMessage versionMsgString
    versionMsgString = Char8.unpack $ showVersionMessage versionMsg
