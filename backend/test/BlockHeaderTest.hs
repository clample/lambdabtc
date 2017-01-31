module BlockHeaderTest where

import TestUtil
import BlockHeaders
import qualified Data.ByteString as BS
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Binary (Binary(..))

instance Arbitrary BlockHeader where
  arbitrary = BlockHeader
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary

instance Arbitrary BlockVersion where
  arbitrary = BlockVersion <$> choose (0, maxVersion)
    where maxVersion = 0xffffffff -- 4 bytes

instance Arbitrary BlockHash where
  arbitrary = BlockHash . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary MerkleRoot where
  arbitrary = MerkleRoot . BS.pack <$> vectorOf 32 arbitrary

instance Arbitrary Timestamp where
  arbitrary = Timestamp . realToFrac <$> (choose (0, maxTime) :: Gen Integer)
    where maxTime = 0xffffffff -- 4 bytes

instance Arbitrary Difficulty where
  arbitrary = Difficulty . BS.pack <$> vectorOf 4 arbitrary

instance Arbitrary Nonce where
  arbitrary = Nonce . BS.pack <$> vectorOf 4 arbitrary

instance Arbitrary TxCount where
  arbitrary = TxCount <$> choose (0, maxCount)
    where maxCount = 0xff -- 1 byte

blockHeaderInvertible = testProperty
  "It should be possible to encode and decode block headers"
  prop_blockHeaderInvertible 

prop_blockHeaderInvertible :: BlockHeader -> Bool
prop_blockHeaderInvertible blockHeader =
  parsedBlockHeader == blockHeader
  where
    parsedBlockHeader = runGet get (runPut . put $ blockHeader)
