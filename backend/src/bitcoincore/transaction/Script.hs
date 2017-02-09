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
import Data.Binary.Put (Put, putWord8, putByteString)
import Data.Binary.Get (Get, getWord8, getByteString, isolate, bytesRead)
import Data.List (reverse)

data Script = Script [ ScriptComponent ]
  deriving (Eq, Show)

data ScriptComponent
  = OP OPCODE
  | Txt ByteString
  deriving (Eq, Show)

data Value = Satoshis Int
  deriving (Eq, Show)

payToPubkeyHash :: PublicKeyRep -> Script
payToPubkeyHash pubKeyRep = Script
  [OP OP_DUP, OP OP_HASH160, Txt (pubKeyHash pubKeyRep)  , OP OP_EQUALVERIFY, OP OP_CHECKSIG]

putScript :: Script -> Put
putScript (Script script) =
  mapM_ putScriptComponent script

putScriptComponent :: ScriptComponent -> Put
putScriptComponent (OP opcode) = putWord8 . fromIntegral . fromEnum $ opcode
putScriptComponent (Txt bs)
  | BS.length bs < 76 = do
      putWord8 . fromIntegral . BS.length $ bs
      putByteString bs
  | otherwise = error "Need to implement OP_PUSHDATA1, OP_PUSHDATA2, etc"


getScript :: Int -> Get Script
getScript lengthBytes = isolate lengthBytes $
  Script . reverse <$> getScriptStep []
  where getScriptStep acc = do
          bytesRead' <- bytesRead
          if  bytesRead' < (fromIntegral lengthBytes)
          then do
            comp <- getScriptComponent
            getScriptStep (comp:acc)
          else return acc

getScriptComponent :: Get ScriptComponent
getScriptComponent = do
  code <- fromIntegral <$> getWord8
  if 0 < code && code < 76
    then Txt <$> getByteString code
    else return $ OP $ toEnum code
