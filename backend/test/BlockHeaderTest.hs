{-# Language OverloadedStrings #-}
module BlockHeaderTest where

import TestUtil
import General.Types
import BitcoinCore.BlockHeaders
import qualified Data.ByteString as BS
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Binary (Binary(..))
import Data.ByteString.Base16 (encode, decode)

blockHeaderInvertible = testProperty
  "It should be possible to encode and decode block headers"
  prop_blockHeaderInvertible 

prop_blockHeaderInvertible :: BlockHeader -> Bool
prop_blockHeaderInvertible blockHeader =
  parsedBlockHeader == blockHeader
  where
    parsedBlockHeader = runGet get (runPut . put $ blockHeader)

genesisBlockHash :: Assertion
genesisBlockHash = assertEqual
  "Genesis block should have the correct hash"
  (hashBlock . genesisBlock $ MainNet)
  (BlockHash . fst . decode $ "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")

genesisBlockTestnetHash :: Assertion
genesisBlockTestnetHash = assertEqual
  "Genesis block for testnet should have the correct hash"
  (hashBlock . genesisBlock $ TestNet3)
  (BlockHash . fst . decode $ "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943")

validHeadersVerify = testProperty
  "ValidHeaders (checksums guaranteed to match) should pass verifyHeaders"
  prop_validHeadersVerify

prop_validHeadersVerify :: ValidHeaders -> Bool
prop_validHeadersVerify (ValidHeaders headers) =
  verifyHeaders headers

newtype ValidHeaders = ValidHeaders [BlockHeader]
  deriving (Show)

instance Arbitrary ValidHeaders where
  arbitrary = do
    oldestHeader <- arbitrary :: Gen BlockHeader
    let header1 = oldestHeader
    header2 <- nextHeader header1
    header3 <- nextHeader header2
    header4 <- nextHeader header3
    let validHeaders = [header1, header2, header3, header4]
    return $ ValidHeaders validHeaders
    where
      nextHeader h =
        BlockHeader
        <$> arbitrary
        <*> pure (hashBlock h)
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
