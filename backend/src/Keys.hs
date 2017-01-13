{-# Language OverloadedStrings #-}
module Keys where

import Prelude hiding (take, concat)
import Data.ByteString (ByteString, append, take, concat)
import Data.ByteString.Char8 (pack)
import Crypto.PubKey.ECC.Types (Curve, getCurveByName, Point(..), CurveName(SEC_p256k1))
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.Hash.Algorithms (SHA256(..), RIPEMD160(..))
import Crypto.PubKey.ECC.ECDSA (PublicKey, public_q, PrivateKey, private_d)
import Crypto.Hash (Digest, digestFromByteString, hashWith)
import Numeric (showHex, readHex)
import Data.Base58String.Bitcoin (Base58String, fromBytes, toBytes)
import Data.Word8 (Word8(..))
import Data.ByteString.Base16 (decode, encode)
import Data.Char (toUpper)

data PublicKeyRep = Compressed ByteString | Uncompressed ByteString
  deriving (Eq)
data PrivateKeyRep = WIF Base58String | Hex ByteString
  deriving (Eq, Show)
data Address = Address Base58String
  deriving (Eq, Show)
type CheckSum = ByteString
type Payload = ByteString
type Prefix = ByteString

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
getAddress pubKeyRep = Address $ encodeBase58Check addressPrefix $ pubKeyHash pubKeyRep

getWIFPrivateKey :: PrivateKeyRep -> PrivateKeyRep
getWIFPrivateKey (Hex privateKey) =
  WIF $ encodeBase58Check privateKeyPrefix privateKey

pubKeyHash :: PublicKeyRep -> ByteString
pubKeyHash pubKeyRep =
  stringToHexByteString $ show $ hashWith RIPEMD160 $ stringToHexByteString $ show $ hashWith SHA256 pubKey
  -- TODO: Is there a cleaner way to compose these hashes
  where
    pubKey = case pubKeyRep of
               Compressed bs -> bs
               Uncompressed bs -> bs


-- The prefix 04 is prepended to uncompressed public keys.
-- The x and y coordinates of the point are represented in hexidecimal and concatenated
-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#public-key-formats
uncompressed :: PublicKey -> PublicKeyRep
uncompressed pubKey =
  Uncompressed $ stringToHexByteString $ "04" ++ showHex x "" ++ showHex y ""
  where
    Point x y = public_q pubKey

-- public keys can be represented using just the x value and the sign of y
-- https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#compressed-public-keys
compressed :: PublicKey -> PublicKeyRep
compressed pubKey =
  Compressed $ stringToHexByteString $ prefix ++ showHex x ""
  where
    Point x y = public_q pubKey
    prefix = case isEven y of
               True -> "02"
               False -> "03"
    isEven n = n `mod` 2 == 0

--https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#base58-and-base58check-encoding
encodeBase58Check :: Prefix -> Payload -> Base58String
encodeBase58Check prefix payload = fromBytes $ concat [withPrefix, base58CheckSum withPrefix]
  where
   withPrefix = prefix `append` payload

base58CheckSum :: ByteString -> ByteString
base58CheckSum payload =
  take 4 $ stringToHexByteString $ show $ hashWith SHA256 $ stringToHexByteString $ show $ hashWith SHA256 payload
  -- TODO: Is there a cleaner way to compose these hashes

decodeBase58Check :: Base58String -> (CheckSum, Payload, Prefix)
decodeBase58Check = undefined
  
addressPrefix :: Prefix
addressPrefix = stringToHexByteString "00"

privateKeyPrefix :: Prefix
privateKeyPrefix = stringToHexByteString "80"



stringToHexByteString :: String -> ByteString
stringToHexByteString = fst . decode . pack 
