module Main where

import Prelude
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Eff (Eff)
import DOM.Event.EventPhase (EventPhase(..))
import Thermite (defaultPerformAction)

data Action =
  OverviewClicked |
  ReceiveFundsClicked |
  RequestFunds |
  SetLabelText String |
  SetAmountText String |
  SetMessageText String

data Context =
  Overview |
  RecieveFunds

type State = {
  context :: Context,
  labelText :: String,
  amountText :: String,
  messageText :: String
  }

initialState :: State
initialState = {
  context: Overview,
  labelText: "",
  amountText: "",
  messageText: ""
  }

render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.p' [ R.text "Welcome to LamdaBTC" ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch OverviewClicked ]
                    [ R.text "Overview" ]
         , R.button [ RP.onClick \_ -> dispatch ReceiveFundsClicked ]
                    [ R.text "ReceiveFunds" ]
        ]
  , mainScreen dispatch state
  ]

mainScreen dispatch { context: Overview } =
  R.p' [ R.text "overview page" ]
mainScreen dispatch { context: RecieveFunds, labelText, amountText, messageText } =
  R.div'
  [ R.p'      [ R.text "RecieveFunds page" ]
  , R.p'      [ R.text "Label:"]
  , R.input   [ RP.className "form-control"
              , RP.placeholder "Label"
              , RP.value labelText
              , RP.onKeyPress \e -> dispatch (SetLabelText e.key)
              ] []
  , R.p'      [ R.text "Amount:"]
  , R.input   [ RP.className "form-control"
              , RP.placeholder "Request amount"
              , RP.value amountText
              , RP.onKeyPress \e -> dispatch (SetAmountText e.key)
              ] []
  , R.select  []
              [ R.option' [ R.text "BTC" ]
              , R.option' [ R.text "mBTC"]
              , R.option' [ R.text "μBTC"]]
  , R.p'      [ R.text "Message:"]
  , R.input   [ RP.className "form-control"
              , RP.placeholder "Message"
              , RP.value messageText
              , RP.onKeyPress \e -> dispatch (SetMessageText e.key)
              ] []
  , R.button  [ RP.onClick \_ -> dispatch RequestFunds ]
              [ R.text "RequestFunds" ]
  ]

performAction :: T.PerformAction _ State _ Action
performAction OverviewClicked _ _ = void (T.cotransform (\state -> state { context = Overview}))
performAction ReceiveFundsClicked _ _ = void (T.cotransform (\state -> state { context = RecieveFunds }))
performAction RequestFunds _ _ = void (T.cotransform (\state -> state))
performAction (SetLabelText s) _ _ = void (T.modifyState (\state -> state { labelText = state.labelText <> s }))
performAction (SetMessageText s) _ _ = void (T.modifyState (\state -> state { messageText = state.messageText <> s }))

-- TODO: Only accept numbers for this input form
performAction (SetAmountText s) _ _ = void (T.modifyState (\state -> state { amountText = state.amountText <> s }))


spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main = T.defaultMain spec initialState unit
