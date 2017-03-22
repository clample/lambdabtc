{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
module BitcoinCore.Keys
  ( PublicKeyRep(..)
  , Address(..)
  , WIFPrivateKey(..)
  , genKeys
  , getAddress
  , getWIFPrivateKey
  , getPrivateKeyFromWIF
  , getPubKey
  , btcCurve
  , serializePrivateKey
  , deserializePrivateKey
  , serializePublicKeyRep
  , deserializePublicKeyRep
  , PubKeyFormat(..)
  , PubKeyHash(..)
  , addressToPubKeyHash
  , hashPubKeyRep
  , addrTxt
  ) where


import General.Util
import General.Types (Network(..))
import General.Hash
  ( Hash(..)
  , hashObject
  , ripemdSha256
  )

import Prelude hiding (take, concat)
import Data.ByteString (ByteString)
import Crypto.PubKey.ECC.Types
  ( Curve
  , getCurveByName
  , Point(..)
  , CurveName(SEC_p256k1)
  )
import Crypto.PubKey.ECC.Generate (generate, generateQ)
import Crypto.PubKey.ECC.ECDSA ( PublicKey(..)
                               , PrivateKey(..))
import Crypto.OpenSSL.ECC
  ( ecGroupFromCurveOID
  , EcGroup
  , ecPointFromOct
  , ecPointToAffineGFp
  )
import qualified Data.Text as T
import Data.Binary (Binary(..))
import Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Control.Lens (makeLenses, (^.))

data PublicKeyRep = PublicKeyRep PubKeyFormat PublicKey
  deriving (Eq, Show)

data PubKeyFormat = Compressed | Uncompressed
  deriving (Eq, Show)

-- WIFPrivateKey and Address have base58 -> use text rep
-- TODO: add base58 type?
newtype WIFPrivateKey  = WIF T.Text
  deriving (Eq, Show)

newtype Address = Address
  { _addrTxt :: T.Text }
  deriving (Eq, Show)

makeLenses ''Address

type PubKeyHash = Hash PublicKeyRep

-- Bitcoin uses a specefic eliptic curve, secp256k1,
-- to generate public private key pairs
btcCurve :: Curve
btcCurve = getCurveByName SEC_p256k1

btcEcGroup :: EcGroup
btcEcGroup = fromMaybe
  (error "Unable to get secp256k1 ec group. This should never happen.")
  (ecGroupFromCurveOID "secp256k1")

genKeys :: IO (PublicKey, PrivateKey)
genKeys = generate btcCurve

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
  where payload = Payload . hash . hashPubKeyRep $ pubKeyRep
        addressPrefix MainNet = Prefix 0x00
        addressPrefix TestNet3 = Prefix 0x6F

addressToPubKeyHash :: Address -> PubKeyHash
addressToPubKeyHash address =
  Hash hash
  where
    (_, Payload hash, _) = decodeBase58Check $ address^.addrTxt

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
privateKeyPrefix = Prefix 0x80

serializePrivateKey :: PrivateKey -> ByteString
serializePrivateKey =
  BL.toStrict
  . Put.runPut
  . Put.putByteString
  . unrollWithPad BE 32
  . fromIntegral
  . private_d

deserializePrivateKey :: ByteString -> PrivateKey
deserializePrivateKey =
  PrivateKey btcCurve
  . roll BE
  . Get.runGet (Get.getByteString 32)
  . getLazyBS
  where
    getLazyBS bs = BL.fromChunks [bs]

instance Binary PublicKeyRep where
  get = deserializePublicKeyRep
  put = serializePublicKeyRep

hashPubKeyRep :: PublicKeyRep -> Hash PublicKeyRep
hashPubKeyRep = hashObject ripemdSha256

serializePublicKeyRep :: PublicKeyRep -> Put

-- See: https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#public-key-formats
serializePublicKeyRep (PublicKeyRep Uncompressed pubKey) =  do
  Put.putWord8  4
  Put.putByteString . unrollWithPad BE 32 $ x
  Put.putByteString . unrollWithPad BE 32 $ y
  where Point x y = public_q pubKey

-- See: https://github.com/bitcoinbook/bitcoinbook/blob/first_edition/ch04.asciidoc#compressed-public-keys
serializePublicKeyRep (PublicKeyRep Compressed pubKey) = do
  Put.putWord8 prefix
  Put.putByteString . unrollWithPad BE 32 $ x
  where
    Point x y = public_q pubKey
    prefix = if isEven y
             then 2
             else 3
    isEven n = n `mod` 2 == 0 

deserializePublicKeyRep :: Get PublicKeyRep
deserializePublicKeyRep = do
  prefix <- Get.lookAhead Get.getWord8
  let pubKeyFormat = case prefix of
        0x04 -> Uncompressed
        0x03 -> Compressed 
        0x02 -> Compressed 
  bs <- Get.getByteString $ repLength pubKeyFormat
  case getPubKey bs of
    Left error -> fail $ "failed deserializing public key: " ++ error
    Right pubKey -> return $ PublicKeyRep pubKeyFormat pubKey
  where
    getPubKey :: ByteString -> Either String PublicKey
    getPubKey bs = do
      ecPoint <- ecPointFromOct btcEcGroup bs
      let (x, y) = ecPointToAffineGFp btcEcGroup ecPoint
          btcPubKey = PublicKey btcCurve (Point x y)
      return btcPubKey
    repLength Uncompressed = 65
    repLength Compressed = 33
