module BitcoinCore.MerkleTrees where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode)
import Data.Binary (Binary(..))
import Data.Binary.Put (Put, putByteString)
import Data.Binary.Get (Get, getByteString)
----
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (vectorOf)

newtype MerkleHash = MerkleHash ByteString
  deriving (Eq)

newtype MerkleFlags = MerkleFlags ByteString
  deriving (Eq)

instance Show MerkleHash where
  show (MerkleHash bs) = "MerkleHash " ++ (show . encode $ bs)

instance Show MerkleFlags where
  show (MerkleFlags bs) = "MerkleFlags " ++ (show . encode $ bs)

instance Binary MerkleHash where
  put = putMerkleHash
  get = getMerkleHash

putMerkleHash :: MerkleHash -> Put
putMerkleHash (MerkleHash bs) =
  putByteString . BS.reverse $ bs

getMerkleHash :: Get MerkleHash
getMerkleHash = MerkleHash . BS.reverse <$> getByteString 32

instance Arbitrary MerkleHash where
  arbitrary = MerkleHash . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary MerkleFlags where
  arbitrary = do
    flagsLength' <- arbitrary
    MerkleFlags . BS.pack <$> vectorOf flagsLength' arbitrary
