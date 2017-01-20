{-# Language OverloadedStrings #-}
module Keys
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
  , getPrivateKeyFromHex ) where

import Prelude hiding (take, concat)
import Data.ByteString (ByteString, append, take, concat)
import Data.ByteString.Char8 (pack)
import Crypto.PubKey.ECC.Types ( Curve
                               , getCurveByName
                               , Point(..)
                               , CurveName(SEC_p256k1))
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.Hash.Algorithms (SHA256(..), RIPEMD160(..))
import Crypto.PubKey.ECC.ECDSA ( PublicKey
                               , public_q
                               , PrivateKey(..)
                               , private_d)
import Crypto.Hash (Digest, digestFromByteString, hashWith)
import Numeric (showHex, readHex)
import Data.Base58String.Bitcoin (Base58String, fromBytes, toBytes, toText)
import Data.Word8 (Word8(..))
import Data.ByteString.Base16 (decode, encode)
import Data.Char (toUpper)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Util

data PublicKeyRep =
  Compressed T.Text
  | Uncompressed T.Text
  deriving (Eq)

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

-- Addresses are generated from public key by
-- SHA256, then RIPEMD160 hashing of the public key
-- Then Base58 encoding the resulting hash
-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#bitcoin-addresses
getAddress :: PublicKeyRep  -> Address 
getAddress pubKeyRep =
  Address $ encodeBase58Check addressPrefix payload
  where payload = pubKeyHash pubKeyRep

getHexPrivateKey :: PrivateKey -> PrivateKeyRep
getHexPrivateKey privateKey =
  Hex $ hexify (private_d privateKey) 32

getPrivateKeyFromHex :: PrivateKeyRep -> PrivateKey
getPrivateKeyFromHex (Hex privateKeyHex) = PrivateKey curve privateNumber
  where
    curve = getCurveByName SEC_p256k1
    privateNumber = fst . head . readHex . show $ privateKeyHex
      -- will throw runtime error if privateKeyHex is not readable hex

getWIFPrivateKey :: PrivateKeyRep -> PrivateKeyRep
getWIFPrivateKey (Hex privateKey) =
  WIF $ encodeBase58Check privateKeyPrefix (textToHexByteString privateKey)

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
  `T.append` hexify x 32 -- Pretty sure 32 should be replaced by 64
  `T.append` hexify y 32 -- The uncompressed key is 65 bytes -> 130 hex characters
  where
    Point x y = public_q pubKey

-- public keys can be represented using just the x value and the sign of y
-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#compressed-public-keys
compressed :: PublicKey -> PublicKeyRep
compressed pubKey =
  Compressed $  prefix `T.append` hexify x 32
  where
    Point x y = public_q pubKey
    prefix = if isEven y
               then "02"
               else "03"
    isEven n = n `mod` 2 == 0

addressPrefix :: Prefix
addressPrefix = stringToHexByteString "00"

privateKeyPrefix :: Prefix
privateKeyPrefix = stringToHexByteString "80"

