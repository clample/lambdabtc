{-# Language OverloadedStrings #-}
module BitcoinCore.Keys
  ( PublicKeyRep(..)
  , PrivateKeyRep(..)
  , Address(..)
  , genKeys
  , getAddress
  , uncompressed
  , compressed
  , stringToHexByteString
  , pubKeyHash
  , getWIFPrivateKey
  , getHexPrivateKey
  , textToHexByteString
  , getPrivateKeyFromHex
  , genKeySet
  , getPrivateKeyFromWIF
  , addressPrefix
  , getPubKey
  , btcCurve) where

import Persistence (KeySet(..))
import Util

import Prelude hiding (take, concat)
import Data.ByteString (ByteString)
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
import Numeric (readHex)
import Data.ByteString.Base16 (encode)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

data PublicKeyRep =
  Compressed T.Text
  | Uncompressed T.Text
  deriving (Eq, Show)

data PrivateKeyRep =
  WIF T.Text
  | Hex T.Text 
  deriving (Eq, Show)

data Address = Address T.Text
  deriving (Eq, Show)

-- Bitcoin uses a specefic eliptic curve, secp256k1,
-- to generate public private key pairs
btcCurve :: Curve
btcCurve = getCurveByName SEC_p256k1

genKeys :: IO (PublicKey, PrivateKey)
genKeys = generate btcCurve

genKeySet :: IO KeySet
genKeySet = do
  (pubKey, privKey) <- liftIO genKeys
  let compressedPub@(Compressed pubKeyText) = compressed pubKey
      (Hex privKeyText) = getHexPrivateKey privKey
      (Address addressText) = getAddress compressedPub
  return (KeySet addressText privKeyText pubKeyText)

getPubKey :: PrivateKey -> PublicKey
getPubKey privKey =
  PublicKey btcCurve pubPoint
  where pubPoint = generateQ btcCurve (private_d privKey)

-- Addresses are generated from public key by
-- SHA256, then RIPEMD160 hashing of the public key
-- Then Base58 encoding the resulting hash
-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#bitcoin-addresses
getAddress :: PublicKeyRep  -> Address 
getAddress pubKeyRep =
  Address $ encodeBase58Check addressPrefix payload
  where payload = Payload $ pubKeyHash pubKeyRep

getHexPrivateKey :: PrivateKey -> PrivateKeyRep
getHexPrivateKey privateKey =
  Hex $ hexify (private_d privateKey) 64

getPrivateKeyFromHex :: PrivateKeyRep -> PrivateKey
getPrivateKeyFromHex (Hex privateKeyHex) = PrivateKey curve privateNumber
  where
    curve = getCurveByName SEC_p256k1
    privateNumber = fst . head . readHex . T.unpack $ privateKeyHex

getPrivateKeyFromHex _ = error "getPrivateKeyFromHex: Unable to get private key"

getWIFPrivateKey :: PrivateKeyRep -> PrivateKeyRep
getWIFPrivateKey (Hex privateKey) =
  WIF $ encodeBase58Check privateKeyPrefix (Payload $ textToHexByteString privateKey)

getWIFPrivateKey _ = error "getWIFPrivateKey: Unable to get WIF private key"

getPrivateKeyFromWIF :: PrivateKeyRep -> PrivateKey
getPrivateKeyFromWIF (WIF privateKeyWIF) = PrivateKey curve privateNumber
  where
    curve = getCurveByName SEC_p256k1
    privateNumber = (fst . head . readHex . unpack . encode) payload
    (_, Payload payload, _) = decodeBase58Check privateKeyWIF

getPrivateKeyFromWIF _ = error "getPrivateKeyFromWIF: Unable to get private key"


pubKeyHash :: PublicKeyRep -> ByteString
pubKeyHash pubKeyRep =
  stringToHexByteString . show . hashWith RIPEMD160 . hashWith SHA256 $ pubKey
  where
    pubKey = textToHexByteString $ case pubKeyRep of
               Compressed bs -> bs
               Uncompressed bs -> bs

-- The prefix 04 is prepended to uncompressed public keys.
-- The x and y coordinates of the point are represented in hexidecimal and concatenated
-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#public-key-formats
uncompressed :: PublicKey -> PublicKeyRep
uncompressed pubKey =
  Uncompressed $
  "04"
  `T.append` hexify x 64 
  `T.append` hexify y 64 
  where
    Point x y = public_q pubKey

-- public keys can be represented using just the x value and the sign of y
-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#compressed-public-keys
compressed :: PublicKey -> PublicKeyRep
compressed pubKey =
  Compressed $  prfx `T.append` hexify x 64
  where
    Point x y = public_q pubKey
    prfx = if isEven y
           then "02"
           else "03"
    isEven n = n `mod` 2 == 0

addressPrefix :: Prefix
addressPrefix = prefix $ stringToHexByteString "00"

privateKeyPrefix :: Prefix
privateKeyPrefix = prefix $ stringToHexByteString "80"

