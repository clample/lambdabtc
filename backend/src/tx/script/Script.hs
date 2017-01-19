{-# LANGUAGE OverloadedStrings #-}
module Script where

import Util
import Optcodes (OPCODE(..))
import Prelude hiding (concat, reverse, sequence)
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

compile :: Script -> CompiledScript -- Binary encoded rather than hex
compile (Script script) = CompiledScript $ BS.concat $ map compileScriptComponent script

compileScriptComponent :: ScriptComponent -> ByteString
compileScriptComponent (OP opcode) = BS.singleton . fromIntegral . fromEnum $ opcode
compileScriptComponent (Txt bs) =
   (payloadLength compiledTextComponent) `BS.append` compiledTextComponent
   where
     compiledTextComponent = fst . decode $ bs
   

blockLockTime :: ByteString -- Binary rather than Hex representation
blockLockTime = fst . decode . pack $ replicate 8 '0'

txVersion :: ByteString               -- Binary rather than Hex representation
txVersion = fst . decode $ "01000000" -- TODO: this should maybe be not hardcoded

inputCount :: Int -> ByteString -- Binary rather than Hex representation
inputCount count = fst . decode . T.encodeUtf8 $ hexify (toInteger count) 2

outputCount :: Int -> ByteString -- Binary rather than Hex representation
outputCount count = fst . decode . T.encodeUtf8 $ hexify (toInteger count) 2
-- TODO: Don't duplicate this code from inputCount

outPoint :: UTXO -> ByteString
outPoint utxo =
  (BS.reverse (outTxHash utxo)) -- 32 Bytes, little endian
  `BS.append`
  (BS.reverse . fst . decode . T.encodeUtf8 $ hexify  (toInteger $ outIndex utxo) 8)
  -- 4 Bytes, pretty sure this is little endian as well
  -- (see: http://bitcoin.stackexchange.com/questions/3374/how-to-redeem-a-basic-tx)

txValue :: Value -> ByteString
txValue (Satoshis i) =  BS.reverse . fst . decode . T.encodeUtf8 $ hexify (toInteger i) 16
  -- should be little endian, hence the BS.reverse
  -- 8 bytes
  
sequence :: ByteString -- Binary rather than Hex representation
sequence = fst . decode . pack $ replicate 8 'f'

-- TODO: handle multiple outputs
-- Maybe this needs to be a hex ByteString before hashing it?
rawTransaction :: UTXO -> PublicKeyRep -> Value -> ByteString
rawTransaction utxo pubKey value = BS.concat
  [ txVersion
  , inputCount 1
  , outPoint utxo -- will probably take some parameter?
  , payloadLength (scriptSig utxo)
  , scriptSig utxo
  , sequence
  , outputCount 1
  , txValue value
  , payloadLength payToPubKeyHashBS
  , payToPubKeyHashBS
  , blockLockTime
  , txVersion
  ]
  where
    CompiledScript payToPubKeyHashBS = payToPubkeyHash pubKey
    


data UTXO = UTXO
  { scriptSig :: ByteString
  , outTxHash :: ByteString
  , outIndex :: Int }

exampleUTXO :: UTXO
exampleUTXO = UTXO
  { outTxHash = fst . decode $ "276cf0cf6a796bd851c3b57fed13276048d54215ccd5973185801603c6fea8db"
  , scriptSig = fst . decode $ "3045022100d4afb7002f5f14dd9392ceb924e589074741cc30162065313e865d01bddc55cf022064ca084b62c8b18a82b8d595571406f9bc326e7929cc955a22c9e5e2cfcc743e01 03785c8ac7dccd26cb87ba404d3e2b7c382c36673db30617db3d3788043f46f6a6"
  , outIndex = 2
  }

examplePubKeyRep :: PublicKeyRep
examplePubKeyRep = Uncompressed 
  "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"

rawExample :: String
rawExample = T.unpack $ T.decodeUtf8 $ encode $ rawTransaction exampleUTXO examplePubKeyRep (Satoshis 100)
