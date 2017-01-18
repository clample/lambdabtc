module Overview where

import Prelude

import Halogen as H
import Halogen.HTML as HH

type OverviewState = { }

initialState :: OverviewState
initialState = { }

data OverviewQuery a
  = GetOverviewState (Boolean -> a)

data OverviewSlot = OverviewSlot
derive instance eqOverviewSlot :: Eq OverviewSlot
derive instance ordOverviewSlot :: Ord OverviewSlot

overviewComponent :: forall m. H.Component HH.HTML OverviewQuery Void m
overviewComponent = H.component { render, eval, initialState }
  where

  render :: OverviewState -> H.ComponentHTML OverviewQuery
  render (state) = HH.div_
    [ HH.h1_ [ HH.text "Overview" ] ]

  eval :: OverviewQuery ~> H.ComponentDSL OverviewState OverviewQuery Void m
  eval (GetOverviewState reply) = do
    s <- H.gets (\state -> true)
    pure (reply s)
