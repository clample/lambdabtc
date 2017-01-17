{-# LANGUAGE OverloadedStrings #-}
module Server.Handlers where

import Network.HTTP.Types.Status (internalServerError500, ok200)
import Data.Aeson (object, (.=), Value (Null))
import Server.Config
import Web.Scotty.Trans (status, showError, json)

defaultH :: Environment -> Error -> Action
defaultH e x = do
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o

postFundRequestsH :: Action
postFundRequestsH = do
  status ok200
