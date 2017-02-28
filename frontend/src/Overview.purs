module Overview where

import Prelude

import Halogen as H
import Halogen.HTML as HH

type OverviewState = { totalFundsMessage :: String }

initialState :: OverviewState
initialState = { totalFundsMessage: "no message recieved" }

data OverviewQuery a
  = GetOverviewState (Boolean -> a)
  | IncomingFunds String a

data OverviewSlot = OverviewSlot
derive instance eqOverviewSlot :: Eq OverviewSlot
derive instance ordOverviewSlot :: Ord OverviewSlot

overviewComponent :: forall m. H.Component HH.HTML OverviewQuery Void m
overviewComponent = H.component { render, eval, initialState }
  where

  render :: OverviewState -> H.ComponentHTML OverviewQuery
  render (state) = HH.div_
    [ HH.h1_ [ HH.text "Overview" ]
    , HH.p_ [ HH.text state.totalFundsMessage]]

  eval :: OverviewQuery ~> H.ComponentDSL OverviewState OverviewQuery Void m
  eval (GetOverviewState reply) = do
    s <- H.gets (\state -> true)
    pure (reply s)
  eval (IncomingFunds msg next) = do
    H.modify (_ { totalFundsMessage = msg })
    pure next
