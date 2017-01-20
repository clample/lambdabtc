module Util where

import Prelude

import Data.Maybe (listToMaybe)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Base58String.Bitcoin (Base58String, fromBytes, toBytes, toText)
import Crypto.Hash.Algorithms (SHA256(..), RIPEMD160(..))
import Crypto.Hash (Digest, digestFromByteString, hashWith)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base16 (decode, encode)
import Numeric (showHex, readHex)

type Payload = ByteString

type Prefix = ByteString



maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

base58CheckSum :: ByteString -> ByteString
base58CheckSum =
  BS.take 4 . stringToHexByteString . show . hashWith SHA256 . hashWith SHA256 

--https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#base58-and-base58check-encoding
encodeBase58Check :: Prefix -> Payload -> T.Text
encodeBase58Check prefix payload =
  toText . fromBytes . BS.concat $ [withPrefix, base58CheckSum withPrefix]
  where
   withPrefix = prefix `BS.append` payload

-- this function name is misleading?
-- This is going from hex string to binary bytestring
stringToHexByteString :: String -> ByteString
stringToHexByteString = fst . decode . pack 

textToHexByteString :: T.Text -> ByteString
textToHexByteString = stringToHexByteString . T.unpack

-- Make sure that we include leading zeroes when converting an int to its string representatin in hexidecimal
hexify :: Integer -> Int -> T.Text
hexify n desiredLength = T.pack $ leadingZeroes ++ base
  where
    base = showHex n ""
    leadingZeroes = replicate (desiredLength - length base) '0'

-- Take a binary encoded payload and give a bytestring with payload length
payloadLength :: ByteString -> ByteString
payloadLength payload =
  fst $ decode $ T.encodeUtf8 $ hexify (toInteger $ BS.length payload) 2
