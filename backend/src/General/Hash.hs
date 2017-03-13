module General.Hash
  ( Hash(..)
  , hashObject
  , doubleSHA
  , CheckSum(..)
  , checksum
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import Data.Binary (Binary(..), Word32)
import qualified Data.Binary as BIN
import Data.Binary.Put (Put, putByteString, putWord32be)
import Data.Binary.Get (Get, getByteString, getWord32be, runGet, getRemainingLazyByteString)
import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.Hash (hashWith)
import Data.ByteArray (convert)

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (vectorOf)

data Hash a = Hash
  { hash :: ByteString }
  deriving (Eq)

instance Show (Hash a) where
  show (Hash hash') = show . encode $ hash'

instance Binary (Hash a) where
  put = putHash
  get = getHash

putHash :: Hash a -> Put
putHash (Hash hash') =
  putByteString . BS.reverse $ hash'

getHash :: Get (Hash a)
getHash =
  Hash . BS.reverse <$> getByteString 32

hashObject :: Binary a => a -> Hash a
hashObject b = Hash $
  BS.reverse . doubleSHA . BL.toStrict . BIN.encode $ b

instance Arbitrary (Hash a) where
  arbitrary = Hash . BS.pack <$> vectorOf 32 arbitrary

doubleSHA :: ByteString -> ByteString
doubleSHA = convert . hashWith SHA256 . hashWith SHA256

newtype CheckSum = CheckSum Word32
  deriving (Show, Eq)

instance Binary CheckSum where
  put = putCheckSum
  get = getCheckSum

putCheckSum :: CheckSum -> Put
putCheckSum (CheckSum cs) = putWord32be cs

getCheckSum :: Get CheckSum
getCheckSum = CheckSum <$> getWord32be

-- TODO: It would be nice to make this function
--       checksum :: Hash a -> CheckSum a
checksum :: ByteString -> CheckSum
checksum bs = flip runGet hashBS $
  do
    cs <- CheckSum <$> getWord32be
    getRemainingLazyByteString
    return cs
  where
    hashBS = BL.fromChunks [doubleSHA bs]
