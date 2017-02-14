{-# Language OverloadedStrings #-}
module BitcoinCore.Keys
  ( PublicKeyRep(..)
  , Address(..)
  , WIFPrivateKey(..)
  , genKeys
  , getAddress
  , stringToHexByteString
  , pubKeyHash
  , getWIFPrivateKey
  , getPrivateKeyFromWIF
  , textToHexByteString
  , genKeySet
  , getPubKey
  , btcCurve
  , serializePrivateKey
  , deserializePrivateKey
  , serializePublicKeyRep
  , deserializePublicKeyRep
  , PubKeyFormat(..)
  ) where

import General.Persistence (KeySet(..))
import General.Util
import General.Types (Network(..))

import Prelude hiding (take, concat)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import Crypto.PubKey.ECC.Types ( Curve
                               , getCurveByName
                               , Point(..)
                               , CurveName(SEC_p256k1))
import Crypto.PubKey.ECC.Generate (generate, generateQ)
import Crypto.Hash.Algorithms (SHA256(..), RIPEMD160(..))
import Crypto.PubKey.ECC.ECDSA ( PublicKey(..)
                               , PrivateKey(..))
import Crypto.Hash (hashWith)
import Crypto.OpenSSL.ECC (ecGroupFromCurveOID, EcGroup, ecPointFromOct, ecPointToAffineGFp)
import Numeric (readHex)
import Data.ByteString.Base16 (encode)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.ByteArray (convert)
import Data.Binary.Put (runPut, putWord32be, putWord8, putByteString)
import Data.Binary.Get (runGet, getWord32be, getWord8, getByteString)
import qualified Data.ByteString.Lazy as BL


data PublicKeyRep = PublicKeyRep PubKeyFormat PublicKey
  deriving (Eq, Show)

data PubKeyFormat = Compressed | Uncompressed
  deriving (Eq, Show)

-- WIFPrivateKey and Address have base58 -> use text rep
-- TODO: add base58 type?
data WIFPrivateKey  = WIF T.Text
  deriving (Eq, Show)

data Address = Address T.Text
  deriving (Eq, Show)

-- Bitcoin uses a specefic eliptic curve, secp256k1,
-- to generate public private key pairs
btcCurve :: Curve
btcCurve = getCurveByName SEC_p256k1

btcEcGroup :: EcGroup
btcEcGroup = case ecGroupFromCurveOID "secp256k1" of
               Just ecGroup -> ecGroup
               Nothing -> error "Unable to get secp256k1 ec group. This should never happen."

genKeys :: IO (PublicKey, PrivateKey)
genKeys = generate btcCurve

genKeySet :: Network -> IO KeySet
genKeySet network = do
  (pubKey, privKey) <- liftIO genKeys
  let WIF privKeyText = getWIFPrivateKey privKey
      (Address addressText) = getAddress (PublicKeyRep Compressed pubKey) network
  return (KeySet addressText privKeyText)

getPubKey :: PrivateKey -> PublicKey
getPubKey privKey =
  PublicKey btcCurve pubPoint
  where pubPoint = generateQ btcCurve (private_d privKey)

-- Addresses are generated from public key by
-- SHA256, then RIPEMD160 hashing of the public key
-- Then Base58 encoding the resulting hash
-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#bitcoin-addresses
getAddress :: PublicKeyRep  -> Network -> Address 
getAddress pubKeyRep network =
  Address $ encodeBase58Check (addressPrefix network) payload
  where payload = Payload $ pubKeyHash pubKeyRep
        addressPrefix MainNet = prefix $ stringToHexByteString "00"
        addressPrefix TestNet3 = prefix $ stringToHexByteString "6F"

-- TODO: Add inverse of getAddress


getWIFPrivateKey :: PrivateKey -> WIFPrivateKey
getWIFPrivateKey privateKey =
  WIF $ encodeBase58Check privateKeyPrefix (Payload . serializePrivateKey $ privateKey)

getPrivateKeyFromWIF :: WIFPrivateKey -> PrivateKey
getPrivateKeyFromWIF (WIF wifText) =
  if prefix == privateKeyPrefix
  then deserializePrivateKey payload
  else error $ "Unable to read WIF PrivateKey. Invalid prefix: " ++ show prefix
  where
    (prefix, Payload payload, checksum) = decodeBase58Check wifText

privateKeyPrefix :: Prefix
privateKeyPrefix = prefix $ stringToHexByteString "80"

serializePrivateKey :: PrivateKey -> ByteString
serializePrivateKey =
  BL.toStrict
  . runPut
  . putByteString
  . unrollWithPad BE 32
  . fromIntegral
  . private_d

deserializePrivateKey :: ByteString -> PrivateKey
deserializePrivateKey =
  PrivateKey btcCurve
  . roll BE
  . runGet (getByteString 32)
  . getLazyBS
  where
    getLazyBS bs = BL.fromChunks [bs]

pubKeyHash :: PublicKeyRep -> ByteString
pubKeyHash =
  convert
  . hashWith RIPEMD160
  . hashWith SHA256
  . serializePublicKeyRep

serializePublicKeyRep :: PublicKeyRep -> ByteString

-- See: https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#public-key-formats
serializePublicKeyRep (PublicKeyRep Uncompressed pubKey) = BL.toStrict . runPut $ do
  putWord8  4
  putByteString . unrollWithPad BE 32 $ x
  putByteString . unrollWithPad BE 32 $ y
  where Point x y = public_q pubKey

-- See: https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#compressed-public-keys
serializePublicKeyRep (PublicKeyRep Compressed pubKey) = BL.toStrict . runPut $ do
  putWord8 prefix
  putByteString . unrollWithPad BE 32 $ x
  where
    Point x y = public_q pubKey
    prefix = if isEven y
             then 2
             else 3
    isEven n = n `mod` 2 == 0 

deserializePublicKeyRep :: ByteString -> Either String PublicKey
deserializePublicKeyRep bs = do
  ecPoint <- ecPointFromOct btcEcGroup (bs)
  let (x, y) = ecPointToAffineGFp btcEcGroup ecPoint
      btcPubKey = PublicKey btcCurve (Point x y)
  return btcPubKey
