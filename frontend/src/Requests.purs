module Requests where

import Prelude
import Network.HTTP.Affjax as AX

server :: String
server = "http://127.0.0.1:49535"

type Effects eff = (ajax :: AX.AJAX | eff)
