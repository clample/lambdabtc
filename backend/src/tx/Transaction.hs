{-# LANGUAGE OverloadedStrings #-}
module Transaction where

import Util
import Script
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
import Crypto.PubKey.ECC.ECDSA (signWith, Signature(..))
import Data.ASN1.BinaryEncoding (DER(..))
import Data.ASN1.Encoding (decodeASN1, encodeASN1)

unspentAmount :: UTXO -> Value
unspentAmount = undefined

data Transaction = Transaction
  { inputs :: [(UTXO, KeySet)]
  , outputs :: [TxOutput]
  }

data TxOutput = TxOutput
  { value :: Value
  , pubKeyRep :: PublicKeyRep }

tipAmount :: Transaction -> Value
tipAmount tx = Satoshis $ amountInput - amountOutput
  where
    Satoshis amountInput = undefined
    Satoshis amountOutput = undefined

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
  (BS.reverse (outTxHash utxo))
  -- 32 Bytes, little endian
  -- http://www.righto.com/2014/02/bitcoins-hard-way-using-raw-bitcoin.html#ref7
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

-- Maybe this needs to be a hex ByteString before hashing it?
rawTransaction :: Transaction -> ByteString
rawTransaction tx@(Transaction inputs outputs) = BS.concat
  [ txVersion
  , inputCount (length inputs)
  , outPoint utxo -- will probably take some parameter?
  , payloadLength (scriptSigUTXO utxo)
  , scriptSigUTXO utxo
  , sequence
  , outputCount (length outputs)
  , txValue val
  , payloadLength payToPubKeyHashBS
  , payToPubKeyHashBS
  , blockLockTime
  , txVersion
  ]
  where
    utxo = fst . head $ inputs
    val = value $ head outputs
    CompiledScript payToPubKeyHashBS = payToPubkeyHash (pubKeyRep $ head outputs)
    


data UTXO = UTXO
  { scriptSigUTXO :: ByteString
  , outTxHash :: ByteString
  , outIndex :: Int }

exampleUTXO :: UTXO
exampleUTXO = UTXO
  { outTxHash = stringToHexByteString "eccf7e3034189b851985d871f91384b8ee357cd47c3024736e5676eb2debb3f2"
  , scriptSigUTXO = stringToHexByteString "76a914010966776006953d5567439e5e39f86a0d273bee88ac"
  , outIndex = 1
  }

examplePubKeyRep :: PublicKeyRep
examplePubKeyRep = Uncompressed 
  "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"

exampleTxOutput = TxOutput {value = Satoshis 99900000, pubKeyRep = examplePubKeyRep}

exampleTransaction = Transaction {inputs = [(exampleUTXO, undefined)], outputs = [exampleTxOutput]}

rawExample :: String
rawExample = T.unpack $ T.decodeUtf8 $ encode $ rawTransaction exampleTransaction




scriptSig :: ByteString -> KeySet -> ByteString
scriptSig rawTx keySet@(KeySet { keySetPrivateKey = privateKey, keySetPublicKey = publicKey}) =
  BS.concat
  [ derSigHashLength
  , signedHashDER
  , publicKeyBSLength
  , publicKeyBS
  ]
  where
  rawTxHash = stringToHexByteString . show . hashWith SHA256 . hashWith SHA256 $ encode rawTx
  signedHash = fromJust $ signWith 100 (getPrivateKeyFromHex $ Hex privateKey) SHA256 rawTxHash
    -- TODO: CHANGE THIS!
    -- signWith should use a random number, not a hardcoded 100
    -- fromJust will cause runtime errors
  signedHashDER = (derSignature signedHash) `BS.append` sighashAll
  derSigHashLength = payloadLength signedHashDER
  publicKeyBS = textToHexByteString publicKey
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
    intCode = stringToHexByteString "02"
    sequenceCode = stringToHexByteString "30"
    xBS = stringToHexByteString $ showHex (sign_r signature) ""
    yBS = stringToHexByteString $ showHex (sign_s signature) ""
      -- There is probably a better way than
      -- reading to hex then to binary
    
sighashAll :: ByteString
sighashAll = stringToHexByteString "01"
