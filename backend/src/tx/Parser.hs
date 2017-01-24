module TX.Parser where

import Text.Megaparsec (Parsec, Dec, count, hexDigitChar, eof, many)
import TX (Transaction(..), TxVersion, switchEndian, UTXO(..), TxOutput(..))
import Script (Value(..), CompiledScript(..))
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Numeric (readHex)
import Script (ScriptComponent(..), Script(..))
import Crypto.PubKey.ECC.ECDSA (signWith, Signature(..))


data ParsedTransaction = ParsedTransaction
  { version :: TxVersion 
  ,  inputs :: [(UTXO, CompiledScript)]
  , outputs :: [(Value, CompiledScript)]
  } deriving (Eq, Show)

parseTransaction :: Parsec Dec String ParsedTransaction
parseTransaction = do
  v <- parseVersion
  inputCount <- parseCount
  inputArray <- count inputCount parseInput
  parseSequence
  outputCount <- parseCount
  outputArray <- count outputCount parseOutput
  parseBlockLockTime
  eof
  return ParsedTransaction
    { version = v
    , inputs = inputArray
    , outputs =  outputArray}
    

parseVersion :: Parsec Dec String TxVersion
parseVersion = do
  version <- count 8 hexDigitChar
  return $ Char8.pack version

parseCount :: Parsec Dec String Int
parseCount = do
  countStr <- count 2 hexDigitChar
  return . readInt . Char8.pack $ countStr

parseInput :: Parsec Dec String (UTXO, CompiledScript)
parseInput = do
  outPoint <- parseOutPoint
  inputScript <- parsePayload
  return (outPoint, CompiledScript inputScript)

parseOutput :: Parsec Dec String (Value, CompiledScript)
parseOutput = do
  val <- parseTxValue
  outputScript <- parsePayload
  return (val, CompiledScript outputScript)

parseOutPoint :: Parsec Dec String UTXO
parseOutPoint = do
  txHash <- switchEndian . Char8.pack <$> count 64 hexDigitChar
  outIndex <- switchEndian . Char8.pack <$> count 8 hexDigitChar
  let parsedIndex = read . Char8.unpack $ outIndex
  return $ UTXO txHash parsedIndex

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

parseScript :: Parsec Dec String Script
parseScript =
  Script <$> many parseScriptComponent

parseScriptStep :: Script -> Parsec Dec String Script
parseScriptStep (Script scriptArr) = do
  undefined
  
parseScriptComponent :: Parsec Dec String ScriptComponent
parseScriptComponent = do
  code <- parseCount
  if ( 0 < code && code < 76)
    then parseTxtComponent code
    else return $ OP $ toEnum code

parseTxtComponent :: Int -> Parsec Dec String ScriptComponent
parseTxtComponent numBytes = do
  let charsToParse = numBytes * 2
  payload <- count (charsToParse) hexDigitChar
  return $ Txt $ Char8.pack payload
