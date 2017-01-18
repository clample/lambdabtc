module RequestFunds where

import Prelude
import Requests
import Data.Argonaut.Core as AG
import Data.Argonaut.Parser as AG
import Data.StrMap as SM
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request as AX
import Network.HTTP.Affjax.Response as AX
import Network.HTTP.StatusCode as AX
import Control.Monad.Aff (Aff)
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readJSON, readProp)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax.Response (ResponseType(..))
import Data.Lens (lens)
import Data.Lens.Zoom (Lens, Lens')
import Data.Lens.Setter (set)
import Data.Array ( (:))

fundRequest :: forall a b r. Lens { fundRequest :: a | r } { fundRequest :: b | r } a b
fundRequest = lens _.fundRequest (_ { fundRequest = _})

labelRaw :: forall a b r. Lens { labelRaw :: a | r } { labelRaw :: b | r } a b
labelRaw = lens _.labelRaw (_ { labelRaw = _})

amountRaw :: forall a b r. Lens { amountRaw :: a | r } { amountRaw :: b | r } a b
amountRaw = lens _.amountRaw (_ { amountRaw = _})

messageRaw :: forall a b r. Lens { messageRaw :: a | r} { messageRaw :: b | r} a b
messageRaw = lens _.messageRaw (_ { messageRaw = _})

new :: RequestFundsState
new = set (fundRequest <<< labelRaw) "100" initialState

--fundRequest <<< labelRaw

type RequestFundsState =
  { on :: Boolean
  , fundRequest :: FundRequestRaw
  , maybeError :: Maybe String
  , fundRequestList :: Array FundRequest }

type FundRequestRaw =
  { labelRaw :: String
  , amountRaw :: String
  , messageRaw :: String}

type FundRequest = String

initialState :: RequestFundsState
initialState =
  { on: false
  , fundRequest:
    { labelRaw: ""
    , amountRaw: ""
    , messageRaw: "" }
  , maybeError: Nothing
  , fundRequestList: [] }

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
          , HP.value state.fundRequest.labelRaw
          , HE.onValueChange (HE.input UpdateLabel)
          , HP.classes [HH.ClassName "form-control"]
          , HP.id_ "labelInput"]
        ]

        , HH.div [ HP.classes [HH.ClassName "form-group"]]
          [ HH.label [HP.for "amountInput"] [HH.text "Amount:"]
          , HH.input
            [ HP.inputType HP.InputText
            , HP.value state.fundRequest.amountRaw
            , HE.onValueChange (HE.input UpdateAmount)
            , HP.classes [HH.ClassName "form-control"]
            , HP.inputType HP.InputNumber
            , HP.id_ "amountInput" ]

            ]
        , HH.div [ HP.classes [HH.ClassName "form-group"]]
          [ HH.label [HP.for "messageInput"] [HH.text "Message:"]
          , HH.input
            [ HP.inputType HP.InputText
            , HP.value state.fundRequest.messageRaw
            , HE.onValueChange (HE.input UpdateMessage)
            , HP.classes [HH.ClassName "form-control"]
            , HP.id_ "messageInput" ]
          ]
      ]
      , HH.button
        [ HE.onClick (HE.input_ SubmitFundRequest)
        , HP.classes [HH.ClassName "btn", HH.ClassName "btn-default"]]
        [ HH.text "Request Funds" ]
      , renderRequests state
    ]



  eval :: RequestFundsQuery ~> H.ComponentDSL RequestFundsState RequestFundsQuery Void (Aff (Effects eff))
  eval (UpdateLabel label next) = do
    H.modify (set (fundRequest <<< labelRaw) label)
    pure next
  eval (UpdateAmount amount next) = do
    H.modify (set (fundRequest <<< amountRaw) amount)
    pure next
  eval (UpdateMessage message next) = do
    H.modify (set (fundRequest <<< messageRaw) message)
    pure next
  eval (SubmitFundRequest next) = do
    state <- H.get
    response <- H.liftAff $ postFundRequest state
    H.modify (appendFundRequestOrError response)
    pure next
  eval (GetRequestFundsState reply) = do
    b <- H.gets (\state -> state.on)
    pure (reply b)

renderRequests :: RequestFundsState -> H.ComponentHTML RequestFundsQuery
renderRequests state = HH.div_
  [ HH.ul_ (map (\fundRequest' -> HH.li_ [ HH.text fundRequest']) state.fundRequestList) ]

appendFundRequestOrError :: AX.AffjaxResponse String -> RequestFundsState -> RequestFundsState
appendFundRequestOrError {status: AX.StatusCode 400, headers: _, response: error } state =
  state { maybeError = Just error}
appendFundRequestOrError {status: AX.StatusCode 200, headers: _, response: fundRequest } state =
  state { fundRequestList = fundRequest:fundRequests }
  where fundRequests = state.fundRequestList
appendFundRequestOrError response state = state


fundRequestToJSON :: FundRequestRaw -> AG.Json
fundRequestToJSON fundRequest = AG.fromObject
  ((SM.insert "labelRaw" (AG.fromString fundRequest.labelRaw)
    <<< SM.insert "amountRaw" (AG.fromString fundRequest.amountRaw)
    <<< SM.insert "messageRaw" (AG.fromString fundRequest.messageRaw)) SM.empty)

postFundRequest :: forall e b. (AX.Respondable b) => RequestFundsState -> AX.Affjax e b
postFundRequest state  = AX.post (server <> "/fundrequests") (fundRequestToJSON state.fundRequest)
