module General.InternalMessaging where

import BitcoinCore.Transaction.Transactions (Transaction(..))

data InternalMessage
  = SendTX Transaction
  deriving (Show, Eq)
