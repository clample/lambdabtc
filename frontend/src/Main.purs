
module Main where

import Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Data.Const (Const)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Halogen.VDom.Driver (runUI)
import Overview (OverviewQuery(..), OverviewSlot(..), overviewComponent)
import RequestFunds (RequestFundsQuery(..), RequestFundsSlot(..), requestFundsComponent)
import SendFunds (SendFundsQuery(..), SendFundsSlot(..), sendFundsComponent)
import Requests (Effects)

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

type ChildQuery = OverviewQuery <\/> RequestFundsQuery <\/> SendFundsQuery <\/> Const Void
type ChildSlot = OverviewSlot \/ RequestFundsSlot \/ SendFundsSlot \/ Void

nav :: forall eff. H.ParentHTML Query ChildQuery ChildSlot (Aff (Effects eff))
nav = HH.nav [HP.classes [HH.ClassName "navbar", HH.ClassName "navbar-default"]]
        [ HH.div [HP.classes [HH.ClassName "navbar-header"]]
            [HH.p [HP.classes [HH.ClassName "navbar-brand"]] [HH.text "LamdaBTC"]]
        , HH.div [HP.classes [HH.ClassName "btn-group"]]
          [ HH.button [ HE.onClick (HE.input_ (ToggleContext OverviewContext)), HP.classes [HH.ClassName "navbar-btn", HH.ClassName "btn", HH.ClassName "btn-default" ] ] [ HH.text "Overview" ]
          , HH.button [ HE.onClick (HE.input_ (ToggleContext SendFundsContext)), HP.classes [HH.ClassName "navbar-btn", HH.ClassName "btn", HH.ClassName "btn-default" ] ] [ HH.text "Send Funds" ]
          , HH.button [ HE.onClick (HE.input_ (ToggleContext RequestFundsContext)), HP.classes [HH.ClassName "navbar-btn", HH.ClassName "btn", HH.ClassName "btn-default"] ] [ HH.text "Request Funds" ]]
        ]

ui :: forall eff. {--Applicative eff =>--} H.Component HH.HTML Query Void (Aff (Effects eff))
ui = H.parentComponent { render, eval, initialState }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (Effects eff))
  render state = HH.div_
    [ nav
    , case state.context of
          OverviewContext -> HH.slot' CP.cp1 OverviewSlot (defer \_ -> overviewComponent) absurd
          RequestFundsContext -> HH.slot' CP.cp2 RequestFundsSlot (defer \_ -> requestFundsComponent) absurd
          SendFundsContext -> HH.slot' CP.cp3 SendFundsSlot (defer \_ -> sendFundsComponent) absurd
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (Effects eff))
  eval (ReadStates next) = do
    a <- H.query' CP.cp1 OverviewSlot (H.request GetOverviewState)
    b <- H.query' CP.cp2 RequestFundsSlot (H.request GetRequestFundsState)
    c <- H.query' CP.cp3 SendFundsSlot (H.request GetSendFundsState)
    H.modify (\state -> state { overviewState = a, requestFundsState = b, sendFundsState = c})
    pure next
  eval (ToggleContext context next) = do
    H.modify (\state -> state {context = context})
    pure next

main :: Eff (H.HalogenEffects (Effects ())) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
