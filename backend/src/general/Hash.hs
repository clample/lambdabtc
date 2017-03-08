module General.Hash
  ( Hash(..)
  , hashObject
  ) where

import General.Util (doubleSHA)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import Data.Binary (Binary(..))
import Data.Binary.Put (Put, putByteString, runPut)
import Data.Binary.Get (Get, getByteString)

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
  BS.reverse . doubleSHA . BL.toStrict . runPut . put $ b

instance Arbitrary (Hash a) where
  arbitrary = Hash . BS.pack <$> vectorOf 32 arbitrary
