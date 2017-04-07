module Overview where

import Prelude

import Requests (Effects, server)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (Affjax, AffjaxResponse, get)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Aff (Aff)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (concatMap, cons)
import Data.Argonaut ( jsonParser
                     , class DecodeJson
                     , decodeJson
                     , (.?)
                     )

newtype UTXO = UTXO { keySetId :: Int
                    , value :: Int
                    , isSpent :: Boolean
                    , confirmations :: Int
                    }

instance decodeJsonUTXO :: DecodeJson UTXO where
  decodeJson json = do
    obj <- decodeJson json
    keySetId <- obj .? "dispKeySetId"
    value <- obj .? "dispValue"
    isSpent <- obj .? "dispIsSpent"
    confirmations <- obj .? "dispConfirmations"
    pure $ UTXO { keySetId: keySetId
                , value: value
                , isSpent: isSpent
                , confirmations: confirmations
                }

type OverviewState = { totalFundsMessages :: Array String
                     , utxoList :: Array UTXO 
                     }

initialState :: Tuple (Array String) (Array UTXO) -> OverviewState
initialState tpl = { totalFundsMessages: fst tpl
                   , utxoList: snd tpl }

data OverviewQuery a
  = IncomingFunds String a
  | UpdateUTXOs a
  | GetOverviewState (Tuple (Array String) (Array UTXO) -> a)

data OverviewSlot = OverviewSlot
derive instance eqOverviewSlot :: Eq OverviewSlot
derive instance ordOverviewSlot :: Ord OverviewSlot

overviewComponent :: forall eff. H.Component HH.HTML OverviewQuery (Tuple (Array String) (Array UTXO)) Void (Aff (Effects eff))
overviewComponent = H.component { render, eval, initialState, receiver }
  where

  receiver :: forall a. Tuple (Array String) (Array UTXO) -> Maybe a
  receiver _ = Nothing

  render :: OverviewState -> H.ComponentHTML OverviewQuery
  render (state) = HH.div_
    [ HH.h1_ [ HH.text "Overview" ]
    , HH.table [HP.classes [HH.ClassName "table"]]
      [ HH.caption_ [HH.text "UTXOs"]
      , HH.thead_
        [ HH.tr_
          [ HH.th_ [HH.text "Key Set"]
          , HH.th_ [HH.text "Value"]
          , HH.th_ [HH.text "Is Spent"]
          , HH.th_ [HH.text "Confirmations"]
          ]
        ]
      , HH.tbody_ (map renderUTXO state.utxoList)
      ]
    , HH.p_ (renderMessages state.totalFundsMessages)
    ]

  eval :: OverviewQuery ~> H.ComponentDSL OverviewState OverviewQuery Void (Aff (Effects eff))
  eval (IncomingFunds msg next) = do
    prevMessages <- H.gets (\s -> s.totalFundsMessages)
    H.modify (\s -> s {totalFundsMessages = (cons msg prevMessages) })
    pure next
  eval (UpdateUTXOs next) = do
    newUTXOs <- H.liftAff getUTXOUpdate
    H.modify (updateUTXOs newUTXOs)
    pure next
  eval (GetOverviewState reply) = do
    a <- H.gets (\s -> s.totalFundsMessages)
    b <- H.gets (\s -> s.utxoList)
    pure (reply $ Tuple a b)

renderMessages :: Array String -> Array (H.ComponentHTML OverviewQuery)
renderMessages [] = [HH.text "No new messages."]
renderMessages xs = concatMap renderMessage xs

renderMessage :: String -> Array (H.ComponentHTML OverviewQuery)
renderMessage str = [HH.text str, HH.br_]

renderUTXO :: UTXO -> H.ComponentHTML OverviewQuery
renderUTXO (UTXO utxo) = HH.tr_
  [ HH.td_ [HH.text $ show utxo.keySetId]
  , HH.td_ [HH.text $ show utxo.value]
  , HH.td_ [HH.text $ show utxo.isSpent]
  , HH.td_ [HH.text $ show utxo.confirmations]
  ] 

getUTXOUpdate :: forall e b. (Respondable b) => Affjax e b
getUTXOUpdate = get (server <> "/utxos")

updateUTXOs :: AffjaxResponse String -> OverviewState -> OverviewState
updateUTXOs {status: StatusCode 200, response: utxoStr } s =
  case (jsonParser utxoStr >>= decodeJson) of
    Left error -> s
    Right utxos -> s { utxoList = utxos }
updateUTXOs _ s = s