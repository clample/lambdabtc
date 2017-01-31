{-# LANGUAGE OverloadedStrings #-}

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
  , payloadLength
  , switchEndian
  , payloadLength'
  , checkSum
  , readInt
  , parseCount
  , parsePayload
  , showBool
  , parseBool
  , getVarInt) where

import Prelude hiding (take)

import Data.Maybe (listToMaybe)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Base58String.Bitcoin (Base58String, fromBytes, toBytes, toText, fromText)
import Crypto.Hash.Algorithms (SHA256(..), RIPEMD160(..))
import Crypto.Hash (Digest(..), digestFromByteString, hashWith)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString, take)
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Base16 (decode, encode)
import Numeric (showHex, readHex)
import Text.Megaparsec (Parsec, Dec, count, hexDigitChar)
import Data.Word (Word32)
import Data.ByteArray (convert)
import Data.Binary.Get(Get(..), getWord8, getWord16le, getWord32le, getWord64le)
import Data.Binary.Put (Put(..), putWord8, putWord16le, putWord32le, putWord64le)

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

checkSum :: ByteString -> ByteString
checkSum = (take 4) . convert . hashWith SHA256 . hashWith SHA256

-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#base58-and-base58check-encoding
encodeBase58Check :: Prefix -> Payload -> T.Text
encodeBase58Check (Prefix prefix) (Payload payload) =
  toText . fromBytes . BS.concat $ [withPrefix, checkSum withPrefix]
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
stringToHexByteString = fst . decode . Char8.pack 

textToHexByteString :: T.Text -> ByteString
textToHexByteString = stringToHexByteString . T.unpack

-- Make sure that we include leading zeroes when converting an int to its string representatin in hexidecimal
hexify :: Integer -> Int -> T.Text
hexify n desiredLength =
  if n >= 0
  then T.pack $ leadingZeroes ++ base
  else error $ "hexify: we can not hexify negative integers " ++ show n
  where
    base = showHex n ""
    leadingZeroes = replicate (desiredLength - length base) '0'

-- Take a hex encoded payload and give a hex bs with payload length
payloadLength :: ByteString -> ByteString
payloadLength = payloadLength' 2

payloadLength' :: Int -> ByteString -> ByteString
payloadLength' padding payload =
  T.encodeUtf8 $ hexify (toInteger . BS.length . fst . decode $ payload) padding


switchEndian :: ByteString -> ByteString
switchEndian = encode . BS.reverse . fst . decode 
  -- converts a hex encoded bytestring from little endian to big endian
  -- (and vice versa)


readInt :: ByteString -> Int
readInt = fst . head . readHex . Char8.unpack

parsePayload :: Parsec Dec String ByteString
parsePayload = do
  length <- parseCount
  payload <- count (length * 2) hexDigitChar
    -- * 2 because we are parsing hex characters but the length is in bytes
  return $ Char8.pack payload

parseCount :: Parsec Dec String Int
parseCount = do
  countStr <- count 2 hexDigitChar
  return . readInt . Char8.pack $ countStr

parseBool :: Parsec Dec String Bool
parseBool = do
  str <- count 2 hexDigitChar
  case str of
    "01" -> return True
    "00" -> return False
    _    -> fail "Unable to parse bool"

showBool :: Bool -> ByteString
showBool True  = "01"
showBool False = "00"


getVarInt :: Get Int
getVarInt = do
  firstByte <- fromIntegral <$> getWord8
  varInt firstByte
  where
    varInt firstByte
      | firstByte < 0xFD  = return firstByte
      | firstByte == 0xFD = fromIntegral <$> getWord16le
      | firstByte == 0xFE = fromIntegral <$> getWord32le 
      | firstByte == 0xFF = fromIntegral <$> getWord64le

putVarInt :: Int -> Put
putVarInt i
  | i < 0xFD =
      putWord8 . fromIntegral $ i
  | i <= 0xFFFF =  do
      putWord8 0xFD
      putWord16le . fromIntegral $ i
  | i <= 0xFFFFFFFF = do
      putWord8 0xFE
      putWord32le . fromIntegral $ i
  | otherwise = do
      putWord8 0xFF
      putWord64le . fromIntegral $ i
