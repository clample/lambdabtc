module Protocol.Parser where

import Text.Megaparsec (Parsec, Dec, hexDigitChar, count, many, manyTill, eof)
import Messages (Header(..), readNetwork, readCommand, Network(..), Command(..))
import qualified Data.ByteString.Char8 as Char8

parseHeader :: Parsec Dec String Header
parseHeader = do
  mNetwork       <- readNetwork . Char8.pack <$> count 8 hexDigitChar
  mCommand       <- readCommand . Char8.pack <$> count 24 hexDigitChar
  messageLength  <- count 8 hexDigitChar
  checksum       <- count 8 hexDigitChar
  message        <- Char8.pack <$> manyTill hexDigitChar eof
  case (mNetwork, mCommand) of
    (Just network, Just command) -> return $ Header network command message
    (_, _)                       -> fail "Unable to parse header"

