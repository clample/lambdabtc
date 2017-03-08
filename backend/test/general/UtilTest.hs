module General.UtilTest where

import TestUtil
import General.Util

newtype ReasonableInteger = ReasonableInteger Integer
  deriving (Show, Eq)

instance Arbitrary ReasonableInteger where
  arbitrary = ReasonableInteger <$> choose (0, 0xffffffff)

unrollRoll = testProperty
  "It should be possible to unroll and roll an integer"
  prop_unrollRoll

prop_unrollRoll :: Endian -> ReasonableInteger -> Bool
prop_unrollRoll endian (ReasonableInteger i) =
  (roll endian . unroll endian $ i) == i
