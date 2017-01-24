{-# LANGUAGE OverloadedStrings #-}

module Messages where

import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as T
import Util (switchEndian, hexify, payloadLength', checkSum)
import Data.ByteString.Base16 (decode, encode)

{--
versionMessage :: ByteString
versionMessage =
  (header versionCommand message) `BS.append` message
  where
    message = BS.concat
      [ version 
      , services
      , timestamp
      , addrRecv
      , addrFrom
      , nonce
      , userAgent
      , startHeight
      , relay]
--}

header :: ByteString -> ByteString -> ByteString
header command message = BS.concat
  [ magicHeaderTestNet3
  , command
  , switchEndian $ payloadLength' 8 message
  , (encode . checkSum . fst . decode) message ]
  

magicHeaderMain :: ByteString
magicHeaderMain = "F9BEB4D9" 

magicHeaderTestNet3 :: ByteString
magicHeaderTestNet3 = "0B110907"

versionCommand :: ByteString
versionCommand = "76657273696F6E0000000000"

headerCheck :: String
headerCheck = Char8.unpack $ header versionCommand
  "62EA0000010000000000000011B2D05000000000010000000000000000000000000000000000FFFF000000000000010000000000000000000000000000000000FFFF0000000000003B2EB35D8CE617650F2F5361746F7368693A302E372E322FC03E0300"
