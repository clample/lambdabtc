module TX.Parser where

import Text.Megaparsec
import TX (Transaction(..), TxVersion)
import qualified Data.ByteString.Char8 as Char8

parseVersion :: Parsec Dec String TxVersion
parseVersion = do
  version <- count 8 hexDigitChar
  return $ Char8.pack version
  -- TODO: 
  -- this is inconsistent with the formatting of tx version in Transaction
  -- TxVersion in Transaction is binary encoded, but this is hex encoded
