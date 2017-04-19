module General.InternalMessaging where

import BitcoinCore.Transaction.Transactions (Transaction(..), Value(..))
import BitcoinCore.Keys (Address)

data InternalMessage
  = SendTX Transaction
  | AddAddress Address
  deriving (Show, Eq)

data UIUpdaterMessage
  = IncomingFunds Value
  | UTXOsUpdated
  | NewBlock
  deriving (Show, Eq)
