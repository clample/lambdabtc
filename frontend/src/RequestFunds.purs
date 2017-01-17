module RequestFunds where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Requests
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)
import Data.Argonaut.Core as AG
import Data.StrMap as SM

type RequestFundsState = { on :: Boolean, label :: String, amount :: String, message :: String }

initialState :: RequestFundsState
initialState = { on: false, label: "", amount: "", message: "" }

data RequestFundsQuery a
  = UpdateLabel String a
  | UpdateAmount String a
  | UpdateMessage String a
  | SubmitFundRequest a
  | GetRequestFundsState (Boolean -> a)

data RequestFundsSlot = RequestFundsSlot
derive instance eqRequestFundsSlot :: Eq RequestFundsSlot
derive instance ordRequestFundsSlot :: Ord RequestFundsSlot

requestFundsComponent :: forall eff. H.Component HH.HTML RequestFundsQuery Void (Aff (Effects eff))
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
      , HH.button
        [ HE.onClick (HE.input_ SubmitFundRequest)
        , HP.classes [HH.ClassName "btn", HH.ClassName "btn-default"]]
        [ HH.text "Request Funds" ]
    ]



  eval :: RequestFundsQuery ~> H.ComponentDSL RequestFundsState RequestFundsQuery Void (Aff (Effects eff))
  eval (UpdateLabel label next) = do
    H.modify (\state -> state { label = label })
    pure next
  eval (UpdateAmount amount next) = do
    H.modify (\state -> state { amount = amount })
    pure next
  eval (UpdateMessage message next) = do
    H.modify (\state -> state { message = message })
    pure next
  eval (SubmitFundRequest next) = do
    state <- H.get
    response <- H.liftAff $ postFundRequest state
    let
      rp :: String
      rp = response.response
    pure next
  eval (GetRequestFundsState reply) = do
    b <- H.gets (\state -> state.on)
    pure (reply b)

fundRequestToJSON :: RequestFundsState -> AG.Json
fundRequestToJSON resetFundsState = AG.fromObject
  ((SM.insert "label" (AG.fromString resetFundsState.label)
    <<< SM.insert "amount" (AG.fromString resetFundsState.amount)
    <<< SM.insert "message" (AG.fromString resetFundsState.message)) SM.empty)

postFundRequest state  = AX.post (server <> "/fundrequests") (fundRequestToJSON state)
