module RequestFunds where

import Prelude
import Requests ( Effects, server)
import Data.Argonaut ( jsonParser
                     , class EncodeJson
                     , class DecodeJson
                     , encodeJson
                     , decodeJson
                     , jsonEmptyObject
                     , (~>)
                     , (:=)
                     , (.?))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (Affjax, AffjaxResponse, post)
import Network.HTTP.Affjax.Response  (class Respondable)
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Aff (Aff)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Lens (lens, set, view, Lens, Lens')
import Data.Maybe (Maybe(..))

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
n = view ( fundRequest <<< _FundRequestRaw ) (initialState unit)

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
    pure $ FundRequest
      { label: label
      , message: message
      , amount: amount
      , address: address
      , requestURI: requestURI}

initialState :: Unit -> RequestFundsState
initialState _ =
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

requestFundsComponent :: forall eff. H.Component HH.HTML RequestFundsQuery Unit Void (Aff (Effects eff))
requestFundsComponent = H.component { render, eval, initialState, receiver }
  where

  receiver :: forall a. Unit -> Maybe a
  receiver _ = Nothing

  render :: RequestFundsState -> H.ComponentHTML RequestFundsQuery
  render state =
    HH.div_
    [ renderFundRequestForm state
    , renderRequests state
    -- TODO: render error messages
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

renderFundRequestForm :: RequestFundsState -> H.ComponentHTML RequestFundsQuery
renderFundRequestForm state =
  HH.div_
  [ HH.h1_ [ HH.text "Request Funds" ]
  , HH.form_
    [ HH.div [ HP.classes [HH.ClassName "form-group"]]
      [ HH.label [HP.for "labelInput"] [HH.text "Label:"]
      , HH.input
        [ HP.type_ HP.InputText
        , HP.value (view (fundRequest <<< _FundRequestRaw <<< labelRaw) state)
        , HE.onValueChange (HE.input UpdateLabel)
        , HP.classes [HH.ClassName "form-control"]
        , HP.id_ "labelInput"]
      ]

      , HH.div [ HP.classes [HH.ClassName "form-group"]]
        [ HH.label [HP.for "amountInput"] [HH.text "Amount:"]
        , HH.input
          [ HP.type_ HP.InputText
          , HP.value (view (fundRequest <<< _FundRequestRaw <<< amountRaw) state)
          , HE.onValueChange (HE.input UpdateAmount)
          , HP.classes [HH.ClassName "form-control"]
          , HP.type_ HP.InputNumber
          , HP.id_ "amountInput" ]

          ]
      , HH.div [ HP.classes [HH.ClassName "form-group"]]
        [ HH.label [HP.for "messageInput"] [HH.text "Message:"]
        , HH.input
          [ HP.type_ HP.InputText
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
  ]

renderRequests :: RequestFundsState -> H.ComponentHTML RequestFundsQuery
renderRequests state = HH.div_
  [ HH.table [HP.classes [HH.ClassName "table"]]
      [ HH.caption_ [HH.text "Requested Payments History"]
      , HH.thead_
        [ HH.tr_
          [ HH.th_ [HH.text "Label"]
          , HH.th_ [HH.text "Message"]
          , HH.th_ [HH.text "Amount"]
          , HH.th_ [HH.text "Address"]
          ]
        ]
      , HH.tbody_ (map renderRequest state.fundRequestList)
      ]
    ]

renderRequest :: FundRequest -> H.ComponentHTML RequestFundsQuery
renderRequest (FundRequest fundRequest') = HH.tr_
  [ HH.th_ [HH.text fundRequest'.label]
  , HH.th_ [HH.text fundRequest'.message]
  , HH.th_ [HH.text (show fundRequest'.amount)]
  , HH.th_ [HH.text fundRequest'.address]
  ]

appendFundRequestOrError :: AffjaxResponse String -> RequestFundsState -> RequestFundsState
appendFundRequestOrError {status: StatusCode 400, response: error } state =
  state { maybeError = Just error}

appendFundRequestOrError {status: StatusCode 200, response: fundRequestString } state =
    case (jsonParser fundRequestString >>= decodeJson) of
        Left error ->
          state { maybeError = Just "failed to parse json"}
        Right fundRequest' ->
          state { fundRequestList = fundRequest':fundRequests }
    where fundRequests = state.fundRequestList

appendFundRequestOrError response state =
  state { maybeError = Just "An unexpected error occurred"}
  -- The server should only return 200 or 400 responses


postFundRequest :: forall e b. (Respondable b) => RequestFundsState -> Affjax e b
postFundRequest state  = post (server <> "/fundrequests") (encodeJson state.fundRequest)
