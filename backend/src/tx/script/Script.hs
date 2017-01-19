module Script where

import Optcodes (OPCODE(..))
import Prelude hiding (concat)
import Data.ByteString (ByteString, concat, singleton)
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
  | Txt T.Text

payToPubkeyHash :: CompiledScript
payToPubkeyHash = compile $ Script [OP OP_DUP, OP OP_HASH160, OP OP_EQUALVERIFY, OP OP_CHECKSIG]

compile :: Script -> CompiledScript
compile (Script script) = CompiledScript $ concat $ map compileScriptComponent script

compileScriptComponent :: ScriptComponent -> ByteString
compileScriptComponent (OP opcode) = singleton . fromIntegral . fromEnum $ opcode
compileScriptComponent (Txt str) =
  fst . decode . T.encodeUtf8 $ str
  -- It might be necessary to try different encodings
  -- need to first include length in hex



