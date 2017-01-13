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

data Action = OverviewClicked | ReceiveFundsClicked | RequestFunds | SetEditText String

data Context =
  Overview |
  RecieveFunds

type State = { context :: Context, valueText :: String }

initialState :: State
initialState = { context: Overview, valueText: "" }

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
mainScreen dispatch { context: RecieveFunds, valueText } =
  R.div'
  [ R.p'      [ R.text "RecieveFunds page" ]
  , R.input   [ RP.className "form-control"
              , RP.placeholder "Create a new task"
              , RP.value valueText
              , RP.onKeyPress \e -> dispatch (SetEditText e.key)
              ] []
  , R.button  [ RP.onClick \_ -> dispatch RequestFunds ]
              [ R.text "RequestFunds" ]
  ]

performAction :: T.PerformAction _ State _ Action
performAction OverviewClicked _ _ = void (T.cotransform (\state -> state { context = Overview}))
performAction ReceiveFundsClicked _ _ = void (T.cotransform (\state -> state { context = RecieveFunds }))
performAction RequestFunds _ _ = void (T.cotransform (\state -> state))
performAction (SetEditText s) _ _ = void (T.modifyState (\state -> state { valueText = state.valueText <> s }))

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main = T.defaultMain spec initialState unit
