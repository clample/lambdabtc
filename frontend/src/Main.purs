
module Main where

import Prelude
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Overview as Overview
import WebSocket as WS
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Var (($=))
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AffjaxResponse, get)
import Network.HTTP.StatusCode (StatusCode(..))
import Overview (OverviewSlot(..), overviewComponent)
import RequestFunds (RequestFundsQuery(..), RequestFundsSlot(..), requestFundsComponent, FundRequest)
import Requests (Effects, server)
import SendFunds (SendFundsQuery, SendFundsSlot(..), sendFundsComponent)


messageListener :: forall eff
     . WS.Connection
    -> (Query ~> Aff (HA.HalogenEffects ( ws :: WS.WEBSOCKET | eff)))
    -> Eff (HA.HalogenEffects (ws :: WS.WEBSOCKET | eff)) Unit
messageListener (WS.Connection socket) query =
  socket.onmessage $= \event -> do
    let msg = WS.runMessage <<< WS.runMessageEvent $ event
    HA.runHalogenAff <<< query <<< H.action <<< IncomingFunds $ msg

type State =
  { requestFundsState :: Array FundRequest
  , context :: Context
  }

initialState :: Unit -> State
initialState _ =
  { requestFundsState: []
  , context: OverviewContext }

data Context =
  OverviewContext |
  SendFundsContext |
  RequestFundsContext

data Query a
  = ToggleContext Context a
  | IncomingFunds String a

type ChildQuery = Overview.OverviewQuery <\/> RequestFundsQuery <\/> SendFundsQuery <\/> Const Void
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

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (Effects eff))
ui = H.parentComponent { initialState, render, eval, receiver }
  where

  receiver :: forall a. Unit -> Maybe a
  receiver _ = Nothing

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (Effects eff))
  render state = HH.div_
    [ nav
    , case state.context of
          OverviewContext -> HH.slot' CP.cp1 OverviewSlot overviewComponent unit absurd
          RequestFundsContext -> HH.slot' CP.cp2 RequestFundsSlot requestFundsComponent (state.requestFundsState) absurd
          SendFundsContext -> HH.slot' CP.cp3 SendFundsSlot sendFundsComponent unit absurd
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (Effects eff))
  eval (ToggleContext context next) = do
    state <- H.get
    case (state.context) of
      RequestFundsContext -> do
        fundRequests' <- H.query' CP.cp2 RequestFundsSlot (H.request GetRequestFundsState)
        H.modify (\s -> s {requestFundsState = fromMaybe [] fundRequests' })
      _                   -> pure unit
    H.modify (\s -> s {context = context})
    pure next
  eval (IncomingFunds msg next) = do
    H.query' CP.cp1 OverviewSlot (H.action $ Overview.IncomingFunds msg)
    pure next

waitForServer :: Aff (HA.HalogenEffects (Effects ())) Unit
waitForServer =  do
  (response :: AffjaxResponse String) <- get (server <> "/status")
  if (response.status /= StatusCode 200)
    then waitForServer
    else pure unit


main :: Eff (HA.HalogenEffects (Effects ())) Unit
main = do
  runHalogenAff waitForServer
  connection <- WS.newWebSocket (WS.URL "ws://127.0.0.1:49536") []
  runHalogenAff do
    body <- awaitBody
    io <- runUI ui unit body
    liftEff $ messageListener connection io.query
