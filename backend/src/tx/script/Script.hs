{-# LANGUAGE OverloadedStrings #-}
module Script where

import Util
import Optcodes (OPCODE(..))
import Prelude hiding (concat, reverse)
import qualified  Data.ByteString as BS
import Data.ByteString (ByteString) -- (ByteString, concat, singleton, append, reverse)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Base16 (decode, encode)
import Keys
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
compileScriptComponent (OP opcode) = BS.singleton . fromIntegral . fromEnum $ opcode
compileScriptComponent (Txt bs) =
   (payloadLength compiledTextComponent) `BS.append` compiledTextComponent
   where
     compiledTextComponent = fst . decode $ bs
   -- It might be necessary to try different encodings

blockLockTime :: ByteString -- Binary rather than Hex representation
blockLockTime = fst . decode . pack $ replicate 8 '0'

txVersion :: ByteString               -- Binary rather than Hex representation
txVersion = fst . decode $ "01000000" -- TODO: this should maybe be not hardcoded

inputCount :: Int -> ByteString -- Binary rather than Hex representation
inputCount count = fst . decode . T.encodeUtf8 $ hexify (toInteger count) 2

outputCount :: Int -> ByteString -- Binary rather than Hex representation
outputCount count = fst . decode . T.encodeUtf8 $ hexify (toInteger count) 2

outPoint :: ByteString
outPoint = outTxHash `BS.append` outIndex
  where
    outTxHash = undefined -- 32 Bytes
    outIndex = undefined -- 4 Bytes

txValue :: Value -> ByteString
txValue (Satoshis i) =  BS.reverse . fst . decode . T.encodeUtf8 $ hexify (toInteger i) 16
  -- should be little endian, hence the BS.reverse
  -- 8 bytes
  
