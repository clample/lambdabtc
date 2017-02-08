{-# LANGUAGE OverloadedStrings #-}
module BitcoinCore.Transaction.Script where

import General.Util
import BitcoinCore.Transaction.Optcodes (OPCODE(..))
import BitcoinCore.Keys

import Prelude hiding (concat, reverse, sequence)
import qualified  Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Data.ByteString.Base16 (encode)

data Script = Script [ ScriptComponent ]
  deriving (Eq, Show)

data CompiledScript = CompiledScript ByteString
  deriving (Eq, Show)

data ScriptComponent
  = OP OPCODE
  | Txt ByteString -- should contain hex
  deriving (Eq, Show)

data Value = Satoshis Int
  -- TODO: Improve this. Handle different units
  -- Make a smart constructor so this is never negative?
  deriving (Eq, Show)

payToPubkeyHash :: PublicKeyRep -> CompiledScript
payToPubkeyHash pubKeyRep = compile $ Script [OP OP_DUP, OP OP_HASH160, Txt (encode $ pubKeyHash pubKeyRep)  , OP OP_EQUALVERIFY, OP OP_CHECKSIG]

compile :: Script -> CompiledScript 
compile (Script script) = CompiledScript $ BS.concat $ map compileScriptComponent script

compileScriptComponent :: ScriptComponent -> ByteString
compileScriptComponent (OP opcode) = T.encodeUtf8 $ hexify (fromIntegral . fromEnum $ opcode) 2
compileScriptComponent (Txt bs) =
   payloadLength compiledTextComponent `BS.append` compiledTextComponent
   where
     compiledTextComponent = bs
