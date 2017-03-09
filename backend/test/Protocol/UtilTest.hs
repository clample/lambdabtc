module Protocol.UtilTest where

import BitcoinCore.BlockHeaders (BlockHeader(..))
import TestUtil
import Protocol.Util

persistentBlockHeaderInvertible = testProperty
  "It should be possible to freely convert between BlockHeader and PersistentBlockheader"
  prop_persistentBlockHeaderInvertible

prop_persistentBlockHeaderInvertible :: BlockHeader -> Bool
prop_persistentBlockHeaderInvertible header =
  header' == header
  where
    header' =  decodeBlockHeader . encodeBlockHeader $ header
