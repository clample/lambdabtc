module SendFunds where

import Prelude

import Halogen as H
import Halogen.HTML as HH

type SendFundsState= { }

initialState :: SendFundsState
initialState = {  }

data SendFundsQuery a
  = GetSendFundsState (Boolean -> a)

data SendFundsSlot = SendFundsSlot
derive instance eqSendFundsSlot :: Eq SendFundsSlot
derive instance ordSendFundsSlot :: Ord SendFundsSlot

sendFundsComponent :: forall m. H.Component HH.HTML SendFundsQuery Void m
sendFundsComponent = H.component { render, eval, initialState }
  where

  render :: SendFundsState -> H.ComponentHTML SendFundsQuery
  render (state) = HH.div_
    [ HH.h1_ [ HH.text "Send Funds" ]
    ]

  eval :: SendFundsQuery ~> H.ComponentDSL SendFundsState SendFundsQuery Void m
  eval (GetSendFundsState reply) = do
    b <- H.gets (\(state) -> true)
    pure (reply b)
