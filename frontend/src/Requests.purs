module Requests where

import Prelude
import Network.HTTP.Affjax as AX
import WebSocket as WS

server :: String
server = "http://127.0.0.1:49535"

type Effects eff = (ajax :: AX.AJAX, ws :: WS.WEBSOCKET | eff)
