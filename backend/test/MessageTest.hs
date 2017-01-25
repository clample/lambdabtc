module MessageTest where

import TestUtil
import Messages (Command(..), commandTable, Network(..), networkTable, Header(..), showHeader)
import Protocol.Parser (parseHeader)
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
    
