
module Main where

import Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Overview (QueryA(..), SlotA(..), componentA)
import RequestFunds (QueryB(..), SlotB(..), componentB)
import SendFunds (QueryC(..), SlotC(..), componentC)
import Control.Monad.Eff (Eff)
import DOM.HTML.HTMLElement (offsetHeight)
import Data.Const (Const)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Halogen.VDom.Driver (runUI)

type State =
  { overviewState :: Maybe Boolean
  , requestFundsState :: Maybe Boolean
  , sendFundsState :: Maybe Boolean
  , context :: Context }

initialState :: State
initialState =
  { overviewState: Nothing
  , requestFundsState: Nothing
  , sendFundsState: Nothing
  , context: OverviewContext }

data Context =
  OverviewContext |
  SendFundsContext |
  RequestFundsContext

data Query a =
  ReadStates a |
  ToggleContext Context a

type ChildQuery = QueryA <\/> QueryB <\/> QueryC <\/> Const Void
type ChildSlot = SlotA \/ SlotB \/ SlotC \/ Void

nav :: forall m. H.ParentHTML Query ChildQuery ChildSlot m
nav = HH.nav [HP.classes [HH.ClassName "navbar", HH.ClassName "navbar-default"]]
        [ HH.div [HP.classes [HH.ClassName "navbar-header"]]
            [HH.p [HP.classes [HH.ClassName "navbar-brand"]] [HH.text "LamdaBTC"]]
        , HH.div [HP.classes [HH.ClassName "btn-group"]]
          [ HH.button [ HE.onClick (HE.input_ (ToggleContext OverviewContext)), HP.classes [HH.ClassName "navbar-btn", HH.ClassName "btn", HH.ClassName "btn-default" ] ] [ HH.text "Overview" ]
          , HH.button [ HE.onClick (HE.input_ (ToggleContext SendFundsContext)), HP.classes [HH.ClassName "navbar-btn", HH.ClassName "btn", HH.ClassName "btn-default" ] ] [ HH.text "Send Funds" ]
          , HH.button [ HE.onClick (HE.input_ (ToggleContext RequestFundsContext)), HP.classes [HH.ClassName "navbar-btn", HH.ClassName "btn", HH.ClassName "btn-default"] ] [ HH.text "Request Funds" ]]
        ]

ui :: forall m. Applicative m => H.Component HH.HTML Query Void m
ui = H.parentComponent { render, eval, initialState }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state = HH.div_
    [ nav
    , HH.div_
      [ case state.context of
          OverviewContext -> HH.slot' CP.cp1 SlotA (defer \_ -> componentA) absurd
          SendFundsContext -> HH.slot' CP.cp2 SlotB (defer \_ -> componentB) absurd
          RequestFundsContext -> HH.slot' CP.cp3 SlotC (defer \_ -> componentC) absurd ]
    , HH.div_ [ HH.text $ "Current states: "
      <> show state.overviewState
      <> " / " <> show state.requestFundsState
      <> " / " <> show state.sendFundsState
      <> " / " <>
        case state.context of
          OverviewContext -> "Overview"
          SendFundsContext -> "Send Funds"
          RequestFundsContext -> "Request Funds"]
    , HH.button [ HE.onClick (HE.input_ ReadStates) ] [ HH.text "Read states" ]
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (ReadStates next) = do
    a <- H.query' CP.cp1 SlotA (H.request GetStateA)
    b <- H.query' CP.cp2 SlotB (H.request GetStateB)
    c <- H.query' CP.cp3 SlotC (H.request GetStateC)
    H.modify (\state -> state { overviewState = a, requestFundsState = b, sendFundsState = c})
    pure next
  eval (ToggleContext context next) = do
    H.modify (\state -> state {context = context})
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
