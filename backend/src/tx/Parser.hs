module TX.Parser where

import Text.Megaparsec
import TX (Transaction(..), TxVersion, switchEndian)
import Script (Value(..))
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Numeric (readHex)
import Crypto.PubKey.ECC.ECDSA (signWith, Signature(..))


parseTransaction :: Parsec Dec String Transaction
parseTransaction = do
  version <- parseVersion
  inputCount <- parseCount
  inputArray <- count inputCount $ do
    outPoint <- parseOutPoint
    inputScript <- parsePayload
    return (outPoint, inputScript)
  parseSequence
  outputCount <- parseCount
  outputArray <- count outputCount $ do
    val <- parseTxValue
    outputScript <- parsePayload
    return (val, outputScript)
  parseBlockLockTime
  eof
  return undefined
    

parseVersion :: Parsec Dec String TxVersion
parseVersion = do
  version <- count 8 hexDigitChar
  return $ Char8.pack version

parseCount :: Parsec Dec String Int
parseCount = do
  countStr <- count 2 hexDigitChar
  return . readInt . Char8.pack $ countStr

parseOutPoint :: Parsec Dec String (ByteString, ByteString)
parseOutPoint = do
  txHash <- switchEndian . Char8.pack <$> count 64 hexDigitChar
  outIndex <- switchEndian . Char8.pack <$> count 8 hexDigitChar
  return (txHash, outIndex)

parsePayload :: Parsec Dec String ByteString
parsePayload = do
  length <- parseCount
  payload <- count (length * 2) hexDigitChar
    -- * 2 because we are parsing hex characters but the length is in bytes
  return $ Char8.pack payload

parseSequence :: Parsec Dec String ByteString
parseSequence = do
  s <- count 8 hexDigitChar
  return $ Char8.pack s

parseTxValue :: Parsec Dec String Value
parseTxValue = do
  val <- switchEndian . Char8.pack <$> count 16 hexDigitChar
  return $ Satoshis $ readInt val

parseBlockLockTime :: Parsec Dec String ByteString
parseBlockLockTime = do
  bs <- switchEndian . Char8.pack <$> count 8 hexDigitChar
  return bs

parseDerSignature :: Parsec Dec String Signature
parseDerSignature = do
  sequenceCode <- count 2 hexDigitChar
  derLength <- parseCount
  parseIntCode
  x <- parsePayload
  parseIntCode
  y <- parsePayload
  return $
    Signature (fromIntegral . readInt $ x) (fromIntegral . readInt $ y)
  where
    parseIntCode = count 2 hexDigitChar

readInt :: ByteString -> Int
readInt = fst . head . readHex . Char8.unpack

