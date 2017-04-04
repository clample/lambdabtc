module Overview where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))
import Data.Array (concatMap, cons)

type OverviewState = { totalFundsMessages :: Array String }

initialState :: Array String -> OverviewState
initialState msgs = { totalFundsMessages: msgs }

data OverviewQuery a
  = IncomingFunds String a

data OverviewSlot = OverviewSlot
derive instance eqOverviewSlot :: Eq OverviewSlot
derive instance ordOverviewSlot :: Ord OverviewSlot

overviewComponent :: forall m. H.Component HH.HTML OverviewQuery (Array String) Void m
overviewComponent = H.component { render, eval, initialState, receiver }
  where

  receiver :: forall a. (Array String) -> Maybe a
  receiver _ = Nothing

  render :: OverviewState -> H.ComponentHTML OverviewQuery
  render (state) = HH.div_
    [ HH.h1_ [ HH.text "Overview" ]
    , HH.p_ (renderMessages state.totalFundsMessages)
    ]
    --, HH.p_ [ HH.text state.totalFundsMessage]]

  eval :: OverviewQuery ~> H.ComponentDSL OverviewState OverviewQuery Void m
  eval (IncomingFunds msg next) = do
    prevMessages <- H.gets (\s -> s.totalFundsMessages)
    H.modify (\s -> s {totalFundsMessages = (cons msg prevMessages) })
    pure next
  -- eval (GetOverviewState reply) = do
  --   b <- H.gets (\s -> s.totalFundsMessages)
  --   pure (reply b)

renderMessages :: Array String -> Array (H.ComponentHTML OverviewQuery)
renderMessages [] = [HH.text "No new messages."]
renderMessages xs = concatMap renderMessage xs

renderMessage :: String -> Array (H.ComponentHTML OverviewQuery)
renderMessage str = [HH.text str, HH.br_]