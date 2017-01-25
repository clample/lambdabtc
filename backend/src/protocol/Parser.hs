{-# LANGUAGE OverloadedStrings #-}

module Protocol.Parser where

import Text.Megaparsec (Parsec, Dec, hexDigitChar, count, many, manyTill, eof, getPosition, setPosition, lookAhead)
import Messages (Header(..), readNetwork, readCommand, Network(..), Command(..), VersionMessage(..), Addr(..))
import qualified Data.ByteString.Char8 as Char8
import Numeric (readHex)
import Util (switchEndian, parsePayload)

parseHeader :: Parsec Dec String Header
parseHeader = do
  mNetwork       <- readNetwork . Char8.pack <$> count 8 hexDigitChar
  mCommand       <- readCommand . Char8.pack <$> count 24 hexDigitChar
  messageLength  <- count 8 hexDigitChar
  checksum       <- count 8 hexDigitChar
  message        <- Char8.pack <$> (lookAhead . flip manyTill eof) hexDigitChar
  case (mNetwork, mCommand) of
    (Just network, Just command) -> return $ Header network command message
    (_, _)                       -> fail "Unable to parse header"

parseVersionMessage :: Parsec Dec String VersionMessage
parseVersionMessage = do
  (Header network _ _) <- parseHeader
  version              <- (fst . head . readHex . Char8.unpack . switchEndian . Char8.pack) <$> count 8 hexDigitChar
  services             <- count 16 hexDigitChar
  timestamp            <- parseTimestamp
  peer                 <- parseAddr
  sender               <- parseAddr
  nonce                <- (fst . head . readHex) <$> count 16 hexDigitChar
  userAgent            <- Char8.unpack <$> parsePayload
  startHeight          <- (fst . head . readHex . Char8.unpack . switchEndian . Char8.pack) <$> count 8 hexDigitChar
  eof
  return $ VersionMessage version network timestamp nonce startHeight sender peer
  where
    parseTimestamp = (fst . head . readHex . Char8.unpack . switchEndian . Char8.pack) <$> count 16 hexDigitChar
  

parseAddr :: Parsec Dec String Addr
parseAddr = do
  count 16  hexDigitChar
  count 24 hexDigitChar -- parse ipv6 magic str
  a <- parseIpComponent
  b <- parseIpComponent
  c <- parseIpComponent
  d <- parseIpComponent
  port <- parsePort
  return $ Addr (a, b, c, d) port
  where
    parseIpComponent = (fst . head . readHex) <$> count 2 hexDigitChar
    parsePort = (fst . head . readHex) <$> count 4 hexDigitChar
