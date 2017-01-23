module Util
  ( Payload(..)
  , prefix
  , Prefix
  , maybeRead
  , encodeBase58Check
  , decodeBase58Check
  , stringToHexByteString
  , textToHexByteString
  , hexify
  , payloadLength) where

import Prelude

import Data.Maybe (listToMaybe)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Base58String.Bitcoin (Base58String, fromBytes, toBytes, toText, fromText)
import Crypto.Hash.Algorithms (SHA256(..), RIPEMD160(..))
import Crypto.Hash (Digest, digestFromByteString, hashWith)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base16 (decode, encode)
import Numeric (showHex, readHex)

data Payload = Payload ByteString
  deriving (Show, Eq)

data Prefix = Prefix ByteString
  deriving (Show, Eq)

prefix :: ByteString -> Prefix
prefix bs =
  if BS.length bs == 1
  then Prefix bs
  else error "Prefix should be 1 byte"
  
data CheckSum = CheckSum ByteString
  deriving (Show, Eq)


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

base58CheckSum :: ByteString -> ByteString
base58CheckSum =
  BS.take 4 . stringToHexByteString . show . hashWith SHA256 . hashWith SHA256 

-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#base58-and-base58check-encoding
encodeBase58Check :: Prefix -> Payload -> T.Text
encodeBase58Check (Prefix prefix) (Payload payload) =
  toText . fromBytes . BS.concat $ [withPrefix, base58CheckSum withPrefix]
  where
   withPrefix = prefix `BS.append` payload

-- all components are binary encoded rather than hex
decodeBase58Check :: T.Text -> (Prefix, Payload, CheckSum)
decodeBase58Check b58 = (Prefix prefix, Payload payload, CheckSum checksum)
  where
    content = (toBytes . fromText) b58
    prefix = (BS.singleton . BS.head) content
    withoutPrefix = BS.drop 1 content
    (payload, checksum) = BS.splitAt (BS.length content - 5) withoutPrefix

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

-- Take a hex encoded payload and give a hex bs with payload length
payloadLength :: ByteString -> ByteString
payloadLength payload =
  T.encodeUtf8 $ hexify (toInteger . BS.length . fst . decode $ payload) 2
