{-# LANGUAGE OverloadedStrings #-}
module Script where

import Util
import Optcodes (OPCODE(..))
import Keys

import Prelude hiding (concat, reverse, sequence)

import qualified  Data.ByteString as BS
import Data.ByteString (ByteString) 
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Base16 (decode, encode)
import Numeric (readHex)
import Data.List (reverse)

data Script = Script [ ScriptComponent ]
data CompiledScript = CompiledScript ByteString
  deriving (Eq, Show)

data ScriptComponent
  = OP OPCODE
  | Txt ByteString -- should contain hex

data Value = Satoshis Int -- TODO: Improve this. Handle different units

payToPubkeyHash :: PublicKeyRep -> CompiledScript
payToPubkeyHash pubKeyRep = compile $ Script [OP OP_DUP, OP OP_HASH160, Txt (encode $ pubKeyHash pubKeyRep)  , OP OP_EQUALVERIFY, OP OP_CHECKSIG]

compile :: Script -> CompiledScript 
compile (Script script) = CompiledScript $ BS.concat $ map compileScriptComponent script

compileScriptComponent :: ScriptComponent -> ByteString
compileScriptComponent (OP opcode) = T.encodeUtf8 $ hexify (fromIntegral . fromEnum $ opcode) 2
compileScriptComponent (Txt bs) =
   (payloadLength compiledTextComponent) `BS.append` compiledTextComponent
   where
     compiledTextComponent = bs
