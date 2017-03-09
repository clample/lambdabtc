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

base58CheckInvertible = testProperty
  "We should be able to convert to and from base 58 check"
  prop_base58CheckInvertible

prop_base58CheckInvertible :: Prefix -> Payload -> Bool
prop_base58CheckInvertible prefix payload  =
  (prefix == decodedPrefix) && (payload == decodedPayload)
  where
    b58 = encodeBase58Check prefix payload
    (decodedPrefix, decodedPayload, _) = decodeBase58Check b58
  
