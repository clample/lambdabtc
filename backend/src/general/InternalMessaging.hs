module General.InternalMessaging where

import BitcoinCore.Transaction.Transactions (Transaction(..))
import BitcoinCore.Keys (Address)

data InternalMessage
  = SendTX Transaction
  | AddAddress Address
  deriving (Show, Eq)
