module MessageTest where

import TestUtil
import Messages (Command(..), commandTable, Network(..), networkTable, Header(..), showHeader, Addr(..), networkAddress)
import Protocol.Parser (parseHeader, parseAddr)
import Text.Megaparsec (parseMaybe)
import qualified Data.ByteString.Char8 as Char8

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
    addrString = Char8.unpack $ networkAddress Nothing addr
      -- TODO: Make test more general by also passing (Just time) to networkAddress
