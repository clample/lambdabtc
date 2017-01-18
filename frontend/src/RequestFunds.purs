module RequestFunds where

import Prelude
import Requests
import Data.Argonaut.Core as AG
import Data.Argonaut.Parser as AG
import Data.Argonaut (class EncodeJson, class DecodeJson, Json, encodeJson, fromArray, decodeJson, jsonEmptyObject, (~>), (:=), (.?))
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
import DOM.HTML.HTMLElement (offsetHeight)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readJSON, readProp)
import Data.Lens (lens)
import Data.Lens.Setter (set)
import Data.Lens.Getter (view)
import Data.Lens.Zoom (Lens, Lens')
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax.Response (ResponseType(..))
import Data.Foreign.Class (readJSON)
import Data.Foreign.Class (class IsForeign)
import Control.Monad.Except (runExcept)

fundRequest :: forall a b r. Lens { fundRequest :: a | r } { fundRequest :: b | r } a b
fundRequest = lens _.fundRequest (_ { fundRequest = _})

labelRaw :: forall a b r. Lens { labelRaw :: a | r } { labelRaw :: b | r } a b
labelRaw = lens _.labelRaw (_ { labelRaw = _})

amountRaw :: forall a b r. Lens { amountRaw :: a | r } { amountRaw :: b | r } a b
amountRaw = lens _.amountRaw (_ { amountRaw = _})

messageRaw :: forall a b r. Lens { messageRaw :: a | r} { messageRaw :: b | r} a b
messageRaw = lens _.messageRaw (_ { messageRaw = _})

_FundRequestRaw :: Lens' FundRequestRaw FundRequestRawRec
_FundRequestRaw = lens (\(FundRequestRaw rec) -> rec) (\_ -> FundRequestRaw)

n :: FundRequestRawRec
n = view ( fundRequest <<< _FundRequestRaw ) initialState
--view (fundRequest <<< _FundRequestRaw <<< labelRaw) state
--new :: RequestFundsState
-- new = set (fundRequest <<< labelRaw <<< _FundRequestRaw) "100" initialState

type RequestFundsState =
  { on :: Boolean
  , fundRequest :: FundRequestRaw
  , maybeError :: Maybe String
  , fundRequestList :: Array FundRequest }

data FundRequestRaw = FundRequestRaw FundRequestRawRec
type FundRequestRawRec =
  { labelRaw :: String
  , amountRaw :: String
  , messageRaw :: String}

instance encodeJsonFundRequestRaw :: EncodeJson FundRequestRaw where
  encodeJson (FundRequestRaw fundRequestRaw)
    =  "amountRaw" := fundRequestRaw.amountRaw
    ~> "messageRaw" := fundRequestRaw.messageRaw
    ~> "labelRaw" := fundRequestRaw.labelRaw
    ~> jsonEmptyObject

newtype FundRequest = FundRequest
  { label :: String
  , message :: String
  , amount :: Number
  , address :: String
  , requestURI :: String
  }

instance decodeJsonFundRequest :: DecodeJson FundRequest where
  decodeJson json = do
    obj <- decodeJson json
    label <- obj .? "label"
    message <- obj .? "message"
    amount <- obj .? "amount"
    address <- obj .? "address"
    requestURI <- obj .? "requestURI"
    pure $ FundRequest { label: label, message: message, amount: amount, address: address, requestURI: requestURI}

initialState :: RequestFundsState
initialState =
  { on: false
  , fundRequest:
    FundRequestRaw
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
  render state =
    HH.div_
    [ HH.h1_ [ HH.text "Request Funds" ]
    , HH.form_
      [ HH.div [ HP.classes [HH.ClassName "form-group"]]
        [ HH.label [HP.for "labelInput"] [HH.text "Label:"]
        , HH.input
          [ HP.inputType HP.InputText
          , HP.value (view (fundRequest <<< _FundRequestRaw <<< labelRaw) state)
          , HE.onValueChange (HE.input UpdateLabel)
          , HP.classes [HH.ClassName "form-control"]
          , HP.id_ "labelInput"]
        ]

        , HH.div [ HP.classes [HH.ClassName "form-group"]]
          [ HH.label [HP.for "amountInput"] [HH.text "Amount:"]
          , HH.input
            [ HP.inputType HP.InputText
            , HP.value (view (fundRequest <<< _FundRequestRaw <<< amountRaw) state)
            , HE.onValueChange (HE.input UpdateAmount)
            , HP.classes [HH.ClassName "form-control"]
            , HP.inputType HP.InputNumber
            , HP.id_ "amountInput" ]

            ]
        , HH.div [ HP.classes [HH.ClassName "form-group"]]
          [ HH.label [HP.for "messageInput"] [HH.text "Message:"]
          , HH.input
            [ HP.inputType HP.InputText
            , HP.value (view (fundRequest <<< _FundRequestRaw <<< messageRaw) state)
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
    H.modify (set (fundRequest <<<  _FundRequestRaw <<< labelRaw) label)
    pure next
  eval (UpdateAmount amount next) = do
    H.modify (set (fundRequest <<< _FundRequestRaw <<< amountRaw) amount)
    pure next
  eval (UpdateMessage message next) = do
    H.modify (set (fundRequest <<< _FundRequestRaw <<< messageRaw) message)
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
  [ HH.ul_ (map renderRequest state.fundRequestList) ]

renderRequest :: FundRequest -> H.ComponentHTML RequestFundsQuery
renderRequest (FundRequest fundRequest) =
  HH.p_ [HH.text fundRequest.label ]

appendFundRequestOrError :: AX.AffjaxResponse String -> RequestFundsState -> RequestFundsState
appendFundRequestOrError {status: AX.StatusCode 400, headers: _, response: error } state =
  state { maybeError = Just error}

appendFundRequestOrError {status: AX.StatusCode 200, headers: _, response: fundRequestString } state =
    case (AG.jsonParser fundRequestString) of
      Left error -> state { maybeError = Just "failed to parse json"}
      Right json ->
        case decodeJson json of
          Left error -> state { maybeError = Just "failed to parse json"}
          Right fundRequest -> state { fundRequestList = fundRequest:fundRequests }
    where fundRequests = state.fundRequestList

appendFundRequestOrError response state = state


postFundRequest :: forall e b. (AX.Respondable b) => RequestFundsState -> AX.Affjax e b
postFundRequest state  = AX.post (server <> "/fundrequests") (encodeJson state.fundRequest)
