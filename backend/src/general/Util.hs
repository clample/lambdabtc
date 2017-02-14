{-# LANGUAGE OverloadedStrings #-}

module General.Util
  ( Payload(..)
  , prefix
  , Prefix(..)
  , maybeRead
  , encodeBase58Check
  , decodeBase58Check
  , stringToHexByteString
  , textToHexByteString
  , hexify
  , checkSum
  , showBool
  , VarInt(..)
  , doubleSHA
  , putWithLength
  , roll
  , unroll
  , unrollWithPad
  , readFromTable
  , Endian(..)) where

import Prelude hiding (take)

import Data.Maybe (listToMaybe)
import Data.ByteString (ByteString, take)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Base16 (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Base58String.Bitcoin (fromBytes, toBytes, toText, fromText)
import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.Hash (hashWith)
import Numeric (showHex, readHex)
import Data.ByteArray (convert)
import Data.Binary (Binary(..))
import Data.Binary.Get(Get, getWord8, getWord16le, getWord32le, getWord64le)
import Data.Binary.Put (Put, putWord8, putWord16le, putWord32le, putWord64le, runPut, putByteString)
import Data.Bits (setBit, shiftR, shiftL, (.|.))
import Data.List (unfoldr)
import Data.Char (toUpper)
import Data.Tuple (swap)


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


-- TODO: Get rid of this abomination!
-- this function name is misleading?
-- This is going from hex string to binary bytestring
stringToHexByteString :: String -> ByteString
stringToHexByteString = fst . decode . Char8.pack 

-- TODO: Get rid of this abomination!
textToHexByteString :: T.Text -> ByteString
textToHexByteString = stringToHexByteString . T.unpack

-- Make sure that we include leading zeroes when converting an int to its string representatin in hexidecimal
-- TODO: Get rid of this abomination!
hexify :: Integer -> Int -> T.Text
hexify n desiredLength =
  if n >= 0
  then T.pack $ leadingZeroes ++ base
  else error $ "hexify: we can not hexify negative integers " ++ show n
  where
    base = showHex n ""
    leadingZeroes = replicate (desiredLength - length base) '0'



putWithLength :: Put -> Put
putWithLength putM = do
  let payload = BL.toStrict . runPut $ putM
  put . VarInt . BS.length $ payload
  putByteString payload

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

data Endian = BE | LE
  deriving (Show, Eq)

-- Taken from src of Data.Binary
-- http://hackage.haskell.org/package/binary-0.4.1/docs/src/Data-Binary.html#Binary
unroll :: Endian -> Integer -> ByteString
unroll LE = BS.pack . unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)
unroll BE = BS.reverse . unroll LE


unrollWithPad :: Endian -> Int -> Integer -> ByteString
unrollWithPad BE paddingBytes i =
  leadingNullBytes `BS.append` base
  where
    base = unroll BE i
    leadingNullBytes = BS.pack $ replicate (paddingBytes - BS.length base) 0
unrollWithPad LE paddingBytes i = BS.reverse $ unrollWithPad BE paddingBytes i

roll :: Endian -> ByteString -> Integer
roll LE = foldr unstep 0 . BS.unpack
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

roll BE = roll LE . BS.reverse

readFromTable :: [(a, ByteString)] -> ByteString -> Maybe a
readFromTable table = lookupInTable . uppercase
  where
    uppercase     = Char8.pack . map toUpper . Char8.unpack
    lookupInTable = flip lookup (map swap table)
