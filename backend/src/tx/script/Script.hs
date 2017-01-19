{-# LANGUAGE OverloadedStrings #-}
module Script where

import Util
import Optcodes (OPCODE(..))
import Prelude hiding (concat)
import Data.ByteString (ByteString, concat, singleton, append)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Base16 (decode, encode)
import Keys
import Numeric (readHex)

data Script = Script [ ScriptComponent ]
data CompiledScript = CompiledScript ByteString
  deriving (Eq, Show)

data ScriptComponent
  = OP OPCODE
  | Txt ByteString -- should contain hex

payToPubkeyHash :: PublicKeyRep -> CompiledScript
payToPubkeyHash pubKeyRep = compile $ Script [OP OP_DUP, OP OP_HASH160, Txt (encode $ pubKeyHash pubKeyRep)  , OP OP_EQUALVERIFY, OP OP_CHECKSIG]

compile :: Script -> CompiledScript
compile (Script script) = CompiledScript $ concat $ map compileScriptComponent script

compileScriptComponent :: ScriptComponent -> ByteString
compileScriptComponent (OP opcode) = singleton . fromIntegral . fromEnum $ opcode
compileScriptComponent (Txt bs) =
   (payloadLength compiledTextComponent) `append` compiledTextComponent
   where
     compiledTextComponent = fst . decode $ bs
       -- . T.encodeUtf8 $ txt
  -- It might be necessary to try different encodings

blockLockTime :: ByteString -- Binary rather than Hex representation
blockLockTime = fst . decode . pack $ replicate 8 '0'

txVersion :: ByteString               -- Binary rather than Hex representation
txVersion = fst . decode $ "01000000" -- TODO: this should maybe be not hardcoded


