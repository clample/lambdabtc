module BitcoinCore.Transaction.Optcodes (OPCODE(..), opcodeTable) where

import Data.Tuple (swap)
import Data.List (lookup)

data OPCODE
  = OP_FALSE
  | OP_PUSHDATA1
  | OP_PUSHDATA2
  | OP_PUSHDATA4
  | OP_1NEGATE
  | OP_TRUE
  | OP_2
  | OP_3
  | OP_4
  | OP_5
  | OP_6
  | OP_7
  | OP_8
  | OP_9
  | OP_10
  | OP_11
  | OP_12
  | OP_13
  | OP_14
  | OP_15
  | OP_16
  | OP_NOP
  | OP_IF
  | OP_NOTIF
  | OP_ELSE
  | OP_ENDIF
  | OP_VERIFY
  | OP_RETURN
  | OP_TOALTSTACK
  | OP_FROMALTSTACK
  | OP_IFDUP
  | OP_DEPTH
  | OP_DROP
  | OP_DUP
  | OP_NIP
  | OP_OVER
  | OP_PICK
  | OP_ROLL
  | OP_ROT
  | OP_SWAP
  | OP_TUCK
  | OP_2DROP
  | OP_2DUP
  | OP_3DUP
  | OP_2OVER
  | OP_2ROT
  | OP_2SWAP
  | OP_CAT
  | OP_SUBSTR
  | OP_LEFT
  | OP_RIGHT
  | OP_SIZE
  | OP_INVERT
  | OP_AND
  | OP_OR
  | OP_XOR
  | OP_EQUAL
  | OP_EQUALVERIFY
  | OP_1ADD
  | OP_1SUB
  | OP_2MUL
  | OP_2DIV
  | OP_NEGATE
  | OP_ABS
  | OP_NOT
  | OP_0NOTEQUAL
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_DIV
  | OP_MOD
  | OP_LSHIFT
  | OP_RSHIFT
  | OP_BOOLAND
  | OP_BOOLOR
  | OP_NUMEQUAL
  | OP_NUMEQUALVERIFY
  | OP_NUMNOTEQUAL
  | OP_LESSTHAN
  | OP_GREATERTHAN
  | OP_LESSTHANOREQUAL
  | OP_GREATERTHANOREQUAL
  | OP_MIN
  | OP_MAX
  | OP_WITHIN
  | OP_RIPEMD160
  | OP_SHA1
  | OP_SHA256
  | OP_HASH160
  | OP_HASH256
  | OP_CODESEPERATOR
  | OP_CHECKSIG
  | OP_CHECKSIGVERIFY
  | OP_CHECKMULTISIG
  | OP_CHECKMULTISIGVERIFY
  | OP_CHECKLOCKTIMEVERIFY
  | OP_CHECKSEQUENCEVERIFY
  | OP_PUBKEYHASH
  | OP_PUBKEY
  | OP_INVALIDOPCODE
  deriving (Eq, Show)

instance Enum OPCODE where
  fromEnum opcode = case flip lookup opcodeTable opcode of
                    Just i -> i
                    Nothing -> error $ "Unable to lookup opcode " ++ show opcode
  toEnum i = case flip lookup (map swap opcodeTable) i of
               Just opcode -> opcode
               Nothing -> error $ "Unable to lookup opcode with i " ++ show i

opcodeTable :: [(OPCODE, Int)]
opcodeTable =
  [ (OP_FALSE, 0)
  , (OP_PUSHDATA1, 76)
  , (OP_PUSHDATA2, 77)
  , (OP_PUSHDATA4, 78)
  , (OP_1NEGATE, 79)
  , (OP_TRUE, 81)
  , (OP_2, 82)
  , (OP_3, 83)
  , (OP_4, 84)
  , (OP_5, 85)
  , (OP_6, 86)
  , (OP_7, 87)
  , (OP_8, 88)
  , (OP_9, 89)
  , (OP_10, 90)
  , (OP_11, 91)
  , (OP_12, 92)
  , (OP_13, 93)
  , (OP_14, 94)
  , (OP_15, 95)
  , (OP_16, 96)
  , (OP_NOP, 97)
  , (OP_IF, 99)
  , (OP_NOTIF, 100)
  , (OP_ELSE, 103)
  , (OP_ENDIF, 104)
  , (OP_VERIFY, 105)
  , (OP_RETURN, 106)
  , (OP_TOALTSTACK, 107)
  , (OP_FROMALTSTACK, 108)
  , (OP_IFDUP, 115)
  , (OP_DEPTH, 116)
  , (OP_DROP, 117)
  , (OP_DUP, 118)
  , (OP_NIP, 119)
  , (OP_OVER, 120)
  , (OP_PICK, 121)
  , (OP_ROLL, 122)
  , (OP_ROT, 123)
  , (OP_SWAP, 124)
  , (OP_TUCK, 125)
  , (OP_2DROP, 109)
  , (OP_2DUP, 110)
  , (OP_3DUP, 111)
  , (OP_2OVER, 112)
  , (OP_2ROT, 113)
  , (OP_2SWAP, 114)
  , (OP_CAT, 126)
  , (OP_SUBSTR, 127)
  , (OP_LEFT, 128)
  , (OP_RIGHT, 129)
  , (OP_SIZE, 130)
  , (OP_INVERT, 131)
  , (OP_AND, 132)
  , (OP_OR, 133)
  , (OP_XOR, 134)
  , (OP_EQUAL, 135)
  , (OP_EQUALVERIFY, 136)
  , (OP_1ADD, 139)
  , (OP_1SUB, 140)
  , (OP_2MUL, 141)
  , (OP_2DIV, 142)
  , (OP_NEGATE, 143)
  , (OP_ABS, 144)
  , (OP_NOT, 145)
  , (OP_0NOTEQUAL, 146)
  , (OP_ADD, 147)
  , (OP_SUB, 148)
  , (OP_MUL, 149)
  , (OP_DIV, 150)
  , (OP_MOD, 151)
  , (OP_LSHIFT, 152)
  , (OP_RSHIFT, 153)
  , (OP_BOOLAND, 154)
  , (OP_BOOLOR, 155)
  , (OP_NUMEQUAL, 156)
  , (OP_NUMEQUALVERIFY, 157)
  , (OP_NUMNOTEQUAL, 158)
  , (OP_LESSTHAN, 159)
  , (OP_GREATERTHAN, 160)
  , (OP_LESSTHANOREQUAL, 161)
  , (OP_GREATERTHANOREQUAL, 162)
  , (OP_MIN, 163)
  , (OP_MAX, 164)
  , (OP_WITHIN, 165)
  , (OP_RIPEMD160, 166)
  , (OP_SHA1, 167)
  , (OP_SHA256, 168)
  , (OP_HASH160, 169)
  , (OP_HASH256, 170)
  , (OP_CODESEPERATOR, 171)
  , (OP_CHECKSIG, 172)
  , (OP_CHECKSIGVERIFY, 173)
  , (OP_CHECKMULTISIG, 174)
  , (OP_CHECKMULTISIGVERIFY, 175)
  , (OP_CHECKLOCKTIMEVERIFY, 177)
  , (OP_CHECKSEQUENCEVERIFY, 178)
  , (OP_PUBKEYHASH, 253)
  , (OP_PUBKEY, 254)
  , (OP_INVALIDOPCODE, 255)]
