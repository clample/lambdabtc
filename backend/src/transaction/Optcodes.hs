module Transaction.Optcodes (OPCODE(..), opcodeTable) where

import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.List (lookup)

data OPCODE
  = OP_DUP
  | OP_HASH160
  | OP_EQUAL
  | OP_EQUALVERIFY
  | OP_CHECKSIG
  deriving (Eq, Show)

instance Enum OPCODE where
  fromEnum = fromJust . flip lookup opcodeTable
  toEnum = fromJust . flip lookup (map swap opcodeTable)

opcodeTable =
  [ (OP_DUP, 118)
  , (OP_HASH160, 169)
  , (OP_EQUALVERIFY, 136)
  , (OP_EQUAL, 135)
  , (OP_CHECKSIG, 172)]
