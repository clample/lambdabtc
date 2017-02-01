{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Transaction where

import Util
import Transaction.Script
import Persistence
import Keys

import Prelude hiding (concat, reverse, sequence)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Base16 (decode, encode)
import Numeric (readHex, showHex)
import Data.List (reverse)
import Data.Maybe (fromJust)
import Crypto.Hash (Digest, digestFromByteString, hashWith)
import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.PubKey.ECC.ECDSA (signWith, Signature(..), PrivateKey(..))
import Data.ASN1.BinaryEncoding (DER(..))
import Data.ASN1.Encoding (decodeASN1, encodeASN1)
import Control.Lens (makeLenses, set, over, mapped, _3)


data TxOutput = TxOutput
  { value :: Value
  , pubKeyRep :: PublicKeyRep
  } deriving (Eq, Show)

type TxVersion = ByteString

type Count = ByteString

data UTXO = UTXO
  { outTxHash :: ByteString
  , outIndex :: Int
  } deriving (Eq, Show)

data Transaction = Transaction
  { __inputs :: [(UTXO, PrivateKey)]
  , __outputs :: [TxOutput]
  , __version :: TxVersion
  } deriving (Eq, Show)

makeLenses ''Transaction

blockLockTime :: ByteString 
blockLockTime = pack $ replicate 8 '0'

defaultVersion :: TxVersion 
defaultVersion = "01000000" 

count :: Int -> Count -- represents the input our output count
count count = T.encodeUtf8 $ hexify (toInteger count) 2

outPoint :: UTXO -> ByteString
outPoint utxo =
  (switchEndian (outTxHash utxo))
  -- 32 Bytes, little endian
  -- http://www.righto.com/2014/02/bitcoins-hard-way-using-raw-bitcoin.html#ref7
  `BS.append`
  (switchEndian . T.encodeUtf8 $ hexify  (toInteger $ outIndex utxo) 8)
  -- 4 Bytes, pretty sure this is little endian as well
  -- (see: http://bitcoin.stackexchange.com/questions/3374/how-to-redeem-a-basic-tx)

txValue :: Value -> ByteString
txValue (Satoshis i) =  switchEndian . T.encodeUtf8 $ hexify (toInteger i) 16
  -- littleEndian, 8 bytes

sequence :: ByteString
sequence = pack $ replicate 8 'f'

showTransaction :: Transaction -> ((UTXO, PrivateKey) -> CompiledScript) -> ByteString
showTransaction tx@(Transaction inputs outputs txVersion) getScript = BS.concat
  [ txVersion
  , count (length inputs)
  , outPoint utxo
  , payloadLength inputScript
  , inputScript
  , sequence
  , count (length outputs)
  , txValue val
  , payloadLength payToPubKeyHashBS
  , payToPubKeyHashBS
  , blockLockTime
  ]
  where
    CompiledScript inputScript = getScript $ head inputs
    (utxo, _) = head inputs
    val = value $ head outputs
    CompiledScript payToPubKeyHashBS = payToPubkeyHash (pubKeyRep $ head outputs)

signedTransaction :: Transaction -> ByteString
signedTransaction tx@(Transaction inputs outputs txVersion) =
  showTransaction tx (\(_, privKey) -> scriptSig fillerTransaction privKey)
  where
    fillerTransaction =  showTransaction tx (\(utxo, _) -> getUtxoScript utxo) `BS.append` txVersion

getUtxoScript :: UTXO -> CompiledScript -- Returns the output script from the given UTXO
getUtxoScript utxo = CompiledScript "76a914010966776006953d5567439e5e39f86a0d273bee88ac"

-- TODO: Move this to Script.hs?
scriptSig :: ByteString -> PrivateKey -> CompiledScript
scriptSig rawTx privKey =
  CompiledScript $ BS.concat
  [ derSigHashLength
  , signedHashDER
  , publicKeyBSLength
  , publicKeyBS
  ]
  where
  rawTxHash = pack . show . hashWith SHA256 . hashWith SHA256 $ rawTx
  signedHash = fromJust $ signWith 100 privKey SHA256 rawTxHash
    -- TODO: CHANGE THIS!
    -- signWith should use a random number, not a hardcoded 100
    -- fromJust will cause runtime errors
  signedHashDER = (derSignature signedHash) `BS.append` sighashAll
  derSigHashLength = payloadLength signedHashDER
  Compressed publicKeyText = compressed . getPubKey $ privKey
  publicKeyBS = T.encodeUtf8 publicKeyText
  publicKeyBSLength = payloadLength publicKeyBS
  
  
-- TODO: Try to use https://hackage.haskell.org/package/asn1-encoding
-- instead of implementing this on my own.
-- Maybe I can make Signature an instance of ASN1Decoding/Encoding  
derSignature :: Signature -> ByteString
derSignature signature = BS.concat
  [ sequenceCode
  , payloadLength sequenceBS
  , sequenceBS
  ]
  where
    sequenceBS = BS.concat
      [ intCode
      , payloadLength xBS
      , xBS
      , intCode
      , payloadLength yBS
      , yBS ]
    intCode = "02"
    sequenceCode = "30"
    xBS = pack $ showPaddedHex (sign_r signature)
    yBS = pack $ showPaddedHex (sign_s signature)
    showPaddedHex i =
      case length str `mod` 2 of
        0 -> str
        1 -> "0" ++ str
      where str = showHex i ""
      -- There is probably a better way than
      -- reading to hex then to binary
    
sighashAll :: ByteString
sighashAll = "01"
