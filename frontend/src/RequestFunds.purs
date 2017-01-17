module RequestFunds where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type RequestFundsState = { on :: Boolean, label :: String, amount :: String, message :: String }

initialState :: RequestFundsState
initialState = { on: false, label: "", amount: "", message: "" }

data RequestFundsQuery a
  = UpdateLabel String a
  | UpdateAmount String a
  | UpdateMessage String a
  | GetRequestFundsState (Boolean -> a)

data RequestFundsSlot = RequestFundsSlot
derive instance eqRequestFundsSlot :: Eq RequestFundsSlot
derive instance ordRequestFundsSlot :: Ord RequestFundsSlot

requestFundsComponent :: forall m. H.Component HH.HTML RequestFundsQuery Void m
requestFundsComponent = H.component { render, eval, initialState }
  where

  render :: RequestFundsState -> H.ComponentHTML RequestFundsQuery
  render state = HH.div_
    [ HH.h1_ [ HH.text "Request Funds" ]
    , HH.form_
      [ HH.div [ HP.classes [HH.ClassName "form-group"]]
        [ HH.label [HP.for "labelInput"] [HH.text "Label:"]
        , HH.input
          [ HP.inputType HP.InputText
          , HP.value state.label
          , HE.onValueChange (HE.input UpdateLabel)
          , HP.classes [HH.ClassName "form-control"]
          , HP.id_ "labelInput"]
        ]

      , HH.div [ HP.classes [HH.ClassName "form-group"]]
        [ HH.label [HP.for "amountInput"] [HH.text "Amount:"]
        , HH.input
          [ HP.inputType HP.InputText
          , HP.value state.amount
          , HE.onValueChange (HE.input UpdateAmount)
          , HP.classes [HH.ClassName "form-control"]
          , HP.inputType HP.InputNumber
          , HP.id_ "amountInput" ]

          ]
        ]

      , HH.div [ HP.classes [HH.ClassName "form-group"]]
        [ HH.label [HP.for "messageInput"] [HH.text "Message:"]
        , HH.input
          [ HP.inputType HP.InputText
          , HP.value state.message
          , HE.onValueChange (HE.input UpdateMessage)
          , HP.classes [HH.ClassName "form-control"]
          , HP.id_ "messageInput" ]
        ]
      ]



  eval :: RequestFundsQuery ~> H.ComponentDSL RequestFundsState RequestFundsQuery Void m
  eval (UpdateLabel label next) = do
    H.modify (\state -> state { label = label })
    pure next
  eval (UpdateAmount amount next) = do
    H.modify (\state -> state { amount = amount })
    pure next
  eval (UpdateMessage message next) = do
    H.modify (\state -> state { message = message })
    pure next
  eval (GetRequestFundsState reply) = do
    b <- H.gets (\state -> state.on)
    pure (reply b)
