module SendFunds where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, jsonEmptyObject, (~>), (:=))
import Data.Argonaut (class EncodeJson)
import Data.Lens (lens, set, view, Lens, Lens')
import Network.HTTP.Affjax (Affjax, AffjaxResponse, post)
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Requests (Effects, server)

sendFundsTx :: forall a b r. Lens { sendFundsTx :: a | r } { sendFundsTx :: b | r} a b
sendFundsTx = lens _.sendFundsTx (_ { sendFundsTx = _ })

recieverAddressRaw :: forall a b r. Lens { recieverAddressRaw :: a | r }  { recieverAddressRaw :: b | r } a b
recieverAddressRaw = lens _.recieverAddressRaw (_ { recieverAddressRaw = _ })

transactionAmountRaw :: forall a b r. Lens { transactionAmountRaw :: a | r } { transactionAmountRaw :: b | r } a b
transactionAmountRaw = lens _.transactionAmountRaw (_ { transactionAmountRaw = _ })

_TransactionRaw :: Lens' TransactionRaw TransactionRawRec
_TransactionRaw = lens (\(TransactionRaw rec) -> rec) (\_ -> TransactionRaw)

type SendFundsState =
  { sendFundsTx :: TransactionRaw }

data TransactionRaw = TransactionRaw TransactionRawRec
type TransactionRawRec =
  { recieverAddressRaw :: String
  , transactionAmountRaw :: String }

instance encodeJsonTransactionRaw :: EncodeJson TransactionRaw where
  encodeJson (TransactionRaw transactionRaw)
    = "recieverAddress" := transactionRaw.recieverAddressRaw
    ~> "transactionAmountRaw" := transactionRaw.transactionAmountRaw
    ~> jsonEmptyObject

initialState :: SendFundsState
initialState =
  { sendFundsTx:
    TransactionRaw
    { recieverAddressRaw: ""
    , transactionAmountRaw: ""}}

data SendFundsQuery a
  = UpdateAddress String a
  | UpdateAmount String a
  | SendFunds a
  | GetSendFundsState (Boolean -> a)

data SendFundsSlot = SendFundsSlot
derive instance eqSendFundsSlot :: Eq SendFundsSlot
derive instance ordSendFundsSlot :: Ord SendFundsSlot

sendFundsComponent :: forall m. H.Component HH.HTML SendFundsQuery Void (Aff (Effects m))
sendFundsComponent = H.component { render, eval, initialState }
  where

  render :: SendFundsState -> H.ComponentHTML SendFundsQuery
  render (state) = HH.div_
    [ renderSendFundsForm state ]

  eval :: SendFundsQuery ~> H.ComponentDSL SendFundsState SendFundsQuery Void (Aff (Effects m))
  eval (GetSendFundsState reply) = do
    b <- H.gets (\(state) -> true)
    pure (reply b)
  eval (UpdateAddress addr next) = do
    H.modify (set (sendFundsTx <<< _TransactionRaw <<< recieverAddressRaw) addr)
    pure next
  eval (UpdateAmount amt next) = do
    H.modify (set (sendFundsTx <<< _TransactionRaw <<< transactionAmountRaw) amt)
    pure next
  eval (SendFunds next) = do
    state <- H.get
    responseObject <- H.liftAff $ postTransaction state
    let str :: String
        str = responseObject.response
    pure next

renderSendFundsForm :: SendFundsState -> H.ComponentHTML SendFundsQuery
renderSendFundsForm state =
  HH.div_
  [ HH.h1_ [ HH.text "Send Funds" ]
  , HH.form_
    [ HH.div [ HP.classes [HH.ClassName "form-group"]]
      [ HH.label [HP.for "addressInput"] [HH.text "Address:"]
      , HH.input
        [ HP.inputType HP.InputText
        , HP.value (view (sendFundsTx <<< _TransactionRaw <<< recieverAddressRaw) state)
        , HE.onValueChange (HE.input UpdateAddress)
        , HP.classes [HH.ClassName "form-control"]
        , HP.id_ "addressInput"]
      ]
      , HH.div [ HP.classes [HH.ClassName "form-group"]]
        [ HH.label [HP.for "amountInput"] [HH.text "Amount:"]
        , HH.input
          [ HP.inputType HP.InputText
          , HP.value (view (sendFundsTx <<< _TransactionRaw <<< transactionAmountRaw) state)
          , HE.onValueChange (HE.input UpdateAmount)
          , HP.classes [HH.ClassName "form-control"]
          , HP.id_ "amountInput"]
        ]
    ]
    , HH.button
      [ HE.onClick (HE.input_ SendFunds)
      , HP.classes [HH.ClassName "btn", HH.ClassName "btn-default"]]
      [ HH.text "Send Funds" ]
  ]

postTransaction :: forall e b. (Respondable b) => SendFundsState -> Affjax e b
postTransaction state = post (server <> "/transactions") (encodeJson state.sendFundsTx)
