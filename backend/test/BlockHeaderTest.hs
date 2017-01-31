module BlockHeaderTest where

import TestUtil
import BlockHeaders
import qualified Data.ByteString as BS
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Binary (Binary(..))

blockHeaderInvertible = testProperty
  "It should be possible to encode and decode block headers"
  prop_blockHeaderInvertible 

prop_blockHeaderInvertible :: BlockHeader -> Bool
prop_blockHeaderInvertible blockHeader =
  parsedBlockHeader == blockHeader
  where
    parsedBlockHeader = runGet get (runPut . put $ blockHeader)
