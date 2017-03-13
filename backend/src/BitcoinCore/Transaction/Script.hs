{-# LANGUAGE OverloadedStrings #-}
module BitcoinCore.Transaction.Script where

import BitcoinCore.Transaction.Optcodes (OPCODE(..))
import BitcoinCore.Keys (PubKeyHash(..))
import General.Hash (Hash(..))

import Prelude hiding (concat, reverse, sequence)
import qualified  Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encode)
import Data.Binary (Binary(..))
import Data.Binary.Put (Put, putWord8, putByteString)
import Data.Binary.Get (Get, getWord8, getByteString, isolate, bytesRead)
import Data.List (reverse)

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (listOf, oneof, choose, vectorOf)

data Script = Script [ ScriptComponent ]
  deriving (Eq, Show)

data ScriptComponent
  = OP OPCODE
  | Txt ByteString
  deriving (Eq)

instance Show ScriptComponent where
  show (OP opcode) = "OP " ++ show opcode
  show (Txt bs) = "Txt " ++ (show . encode) bs

instance Binary ScriptComponent where
  put = putScriptComponent
  get = getScriptComponent

payToPubkeyHash :: PubKeyHash -> Script
payToPubkeyHash pubKeyHash = Script
  [OP OP_DUP, OP OP_HASH160, (Txt . hash) pubKeyHash  , OP OP_EQUALVERIFY, OP OP_CHECKSIG]

putScript :: Script -> Put
putScript (Script script) =
  mapM_ put script

putScriptComponent :: ScriptComponent -> Put
putScriptComponent (OP opcode) = putWord8 . fromIntegral . fromEnum $ opcode
putScriptComponent (Txt bs)
  | BS.length bs < 76 = do
      putWord8 . fromIntegral . BS.length $ bs
      putByteString bs
  | otherwise = error $ "Need to implement OP_PUSHDATA1, OP_PUSHDATA2, etc. BS: " ++ (show . encode) bs


getScript :: Int -> Get Script
getScript lengthBytes = isolate lengthBytes $
  Script . reverse <$> getScriptStep []
  where getScriptStep acc = do
          bytesRead' <- bytesRead
          if  bytesRead' < (fromIntegral lengthBytes)
          then do
            comp <- get
            getScriptStep (comp:acc)
          else return acc

getScriptComponent :: Get ScriptComponent
getScriptComponent = do
  code <- fromIntegral <$> getWord8
  if 0 < code && code < 76
    then Txt <$> getByteString code
    else return $ OP $ toEnum code

instance Arbitrary Script where
  arbitrary = Script <$> listOf arbitrary

instance Arbitrary ScriptComponent where
  arbitrary = oneof [genTxt, genOp]
    where
      genTxt = do
        txtLength <- choose (1, 75)
        Txt . BS.pack <$> vectorOf txtLength arbitrary
      genOp = OP <$> arbitrary
