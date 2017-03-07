module MessageTest where

import TestUtil
import General.Types
import General.Util
import Protocol.Messages (Message(..), MessageBody(..), MessageContext(..))
import Protocol.Network (Addr(..))
import Protocol.MessageBodies
import Protocol.Util
import qualified Data.ByteString.Char8 as Char8
import Data.Time.Clock (NominalDiffTime(..))
import Data.Binary (Binary(..))
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Base16 (encode, decode)
import BitcoinCore.BlockHeaders
import BitcoinCore.BloomFilter
import BitcoinCore.Inventory

messageInvertible = testProperty
  "It should be possible to encode and decode messages"
  prop_messageInvertible

prop_messageInvertible :: Message -> Bool
prop_messageInvertible message =
      parsedMessage == message
  where
    parsedMessage = runGet get (runPut . put $ message)
    
