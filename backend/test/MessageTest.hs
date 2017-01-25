module MessageTest where

import TestUtil
import Messages (Command(..), commandTable, Network(..), networkTable)
import Protocol.Parser ()

instance Arbitrary Command where
  arbitrary = do
    let commands = map fst commandTable
    elements commands

instance Arbitrary Network where
  arbitrary = do
    let networks = map fst networkTable
    elements networks

messageHeaderInvertible = testProperty
  "It should be possible to encode and parse a message header"
  prop_messageHeaderInvertible

prop_messageHeaderInvertible :: Network -> Command -> Bool
prop_messageHeaderInvertible = undefined
