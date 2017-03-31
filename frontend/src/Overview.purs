module Overview where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))

type OverviewState = { totalFundsMessage :: String }

initialState :: Unit -> OverviewState
initialState = pure { totalFundsMessage: "no message recieved" }

data OverviewQuery a
  = IncomingFunds String a

data OverviewSlot = OverviewSlot
derive instance eqOverviewSlot :: Eq OverviewSlot
derive instance ordOverviewSlot :: Ord OverviewSlot

overviewComponent :: forall m. H.Component HH.HTML OverviewQuery Unit Void m
overviewComponent = H.component { render, eval, initialState, receiver }
  where

  receiver :: forall a. Unit -> Maybe a
  receiver _ = Nothing

  render :: OverviewState -> H.ComponentHTML OverviewQuery
  render (state) = HH.div_
    [ HH.h1_ [ HH.text "Overview" ]
    , HH.p_ [ HH.text state.totalFundsMessage]]

  eval :: OverviewQuery ~> H.ComponentDSL OverviewState OverviewQuery Void m
  eval (IncomingFunds msg next) = do
    H.modify (_ { totalFundsMessage = msg })
    pure next
