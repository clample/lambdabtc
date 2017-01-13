module Main where

import Prelude

import ReactDOM as RDOM
import React as R
import React.DOM as R
import React.DOM.Props as RP

import Thermite as T
import Control.Monad.Eff (Eff)

data Action = OverviewClicked | ReceiveFundsClicked

data Context =
  Overview |
  RecieveFunds

type State = { context :: Context }

initialState :: State
initialState = { context: Overview }

render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.p' [ R.text "Welcome to LamdaBTC" ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch OverviewClicked ]
                    [ R.text "Overview" ]
         , R.button [ RP.onClick \_ -> dispatch ReceiveFundsClicked ]
                    [ R.text "ReceiveFunds" ]
        ]
  , mainScreen state
  ]

mainScreen { context: Overview } =
  R.p' [ R.text "overview page" ]
mainScreen { context: RecieveFunds } =
  R.p' [ R.text "RecieveFunds page" ]

performAction :: T.PerformAction _ State _ Action
performAction OverviewClicked _ _ = void (T.cotransform (\state -> state { context = Overview}))
performAction ReceiveFundsClicked _ _ = void (T.cotransform (\state -> state { context = RecieveFunds }))

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main = T.defaultMain spec initialState unit
