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
  , VarInt(..)
  , doubleSHA) where

import Prelude hiding (take)

import Data.Maybe (listToMaybe)
import Data.ByteString (ByteString, take)
import Data.ByteString.Base16 (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Base58String.Bitcoin (fromBytes, toBytes, toText, fromText)
import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.Hash (hashWith)
import Numeric (showHex, readHex)
import Text.Megaparsec (Parsec, Dec, count, hexDigitChar)
import Data.ByteArray (convert)
import Data.Binary (Binary(..))
import Data.Binary.Get(Get, getWord8, getWord16le, getWord32le, getWord64le)
import Data.Binary.Put (Put, putWord8, putWord16le, putWord32le, putWord64le)

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
checkSum = take 4 . doubleSHA

-- 
doubleSHA :: ByteString -> ByteString
doubleSHA = convert . hashWith SHA256 . hashWith SHA256
  
-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#base58-and-base58check-encoding
encodeBase58Check :: Prefix -> Payload -> T.Text
encodeBase58Check (Prefix prefix) (Payload payload) =
  toText . fromBytes . BS.concat $ [withPrefix, checkSum withPrefix]
  where
   withPrefix = prefix `BS.append` payload

-- all components are binary encoded rather than hex
decodeBase58Check :: T.Text -> (Prefix, Payload, CheckSum)
decodeBase58Check b58 = (Prefix pre, Payload payload, CheckSum checksum)
  where
    content = (toBytes . fromText) b58
    pre = (BS.singleton . BS.head) content
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
  lengthBytes <- parseCount
  payload <- count (lengthBytes * 2) hexDigitChar
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

newtype VarInt = VarInt Int
  deriving (Eq, Show)

instance Binary VarInt where
  get = getVarInt
  put = putVarInt

getVarInt :: Get VarInt
getVarInt = do
  firstByte <- fromIntegral <$> getWord8
  VarInt <$> varInt firstByte
  where
    varInt firstByte
      | firstByte < 0xFD  = return firstByte
      | firstByte == 0xFD = fromIntegral <$> getWord16le
      | firstByte == 0xFE = fromIntegral <$> getWord32le 
      | firstByte == 0xFF = fromIntegral <$> getWord64le
      | otherwise = error "Unable to parse VarInd. This should not happen"

putVarInt :: VarInt -> Put
putVarInt (VarInt i)
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
