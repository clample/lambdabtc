module Main where


import Prelude
import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Control.Monad.Eff (Eff)
import DOM.HTML.HTMLElement (offsetHeight)
import Halogen.Util (awaitBody, runHalogenAff)

type State =
  { context :: Context }

data Context =
  Overview |
  RecieveFunds

data Query a =
  ToggleContext Context a |
  GetContext (Context -> a)

initialState :: State
initialState = { context: Overview }

mainComponent :: forall g. Component State Query g
mainComponent = component { render, eval }
  where
    render :: State -> ComponentHTML Query
    render state =
      H.div_
        [ H.h1_
            [ H.text "LamdaBTC" ]
        , H.button
            [ E.onClick (E.input_ (ToggleContext Overview)) ]
            [ H.text "Overview" ]
        , H.button
            [ E.onClick (E.input_ (ToggleContext RecieveFunds)) ]
            [ H.text "Recieve Funds"]
        , H.p_ [ H.text case state.context of
                  Overview -> "Overview"
                  RecieveFunds -> "Recieve Funds"]
        ]

    eval :: Query ~> ComponentDSL State Query g
    eval (ToggleContext ct next) = do
      modify (\state -> { context: ct })
      pure next
    eval (GetContext continue) = do
      context <- gets _.context
      pure (continue context)

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI mainComponent initialState body
