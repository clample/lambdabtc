{-# Language OverloadedStrings #-}
module BitcoinCore.BlockHeadersTest where

import TestUtil
import General.Types
import BitcoinCore.BlockHeaders
import General.Hash (Hash(..))

import qualified Data.ByteString as BS
import qualified Data.Binary as BIN
import qualified Data.ByteString.Base16 as B16

blockHeaderInvertible = testProperty
  "It should be possible to encode and decode block headers"
  prop_blockHeaderInvertible 

prop_blockHeaderInvertible :: BlockHeader -> Bool
prop_blockHeaderInvertible blockHeader =
  parsedBlockHeader == blockHeader
  where
    parsedBlockHeader = BIN.decode . BIN.encode $ blockHeader

genesisBlockHash :: Assertion
genesisBlockHash = assertEqual
  "Genesis block should have the correct hash"
  (hashBlock . genesisBlock $ MainNet)
  (Hash . BS.reverse . fst . B16.decode $ "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")

genesisBlockTestnetHash :: Assertion
genesisBlockTestnetHash = assertEqual
  "Genesis block for testnet should have the correct hash"
  (hashBlock . genesisBlock $ TestNet3)
  (Hash . BS.reverse . fst . B16.decode $ "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943")

validHeadersVerify = testProperty
  "ValidHeaders (checksums guaranteed to match) should pass verifyHeaders"
  prop_validHeadersVerify

prop_validHeadersVerify :: ValidHeaders -> Bool
prop_validHeadersVerify (ValidHeaders headers) =
  verifyHeaders headers