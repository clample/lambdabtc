{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module LamdaBTC.Handlers
  ( defaultH
  , postFundRequestsH
  , getFundRequestsH
  ) where

import LamdaBTC.Config
import BitcoinCore.Keys
import Persistence

import Network.HTTP.Types.Status (internalServerError500, ok200, badRequest400)
import Data.Aeson ( object
                  , (.=)
                  , Value (Null)
                  , FromJSON
                  , ToJSON
                  , toEncoding
                  , genericToEncoding
                  , defaultOptions)
import Web.Scotty.Trans (status, showError, json, jsonData)
import GHC.Generics
import Util (maybeRead)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T 
import Database.Persist.Sql (insert_, selectList)
import Database.Persist (Entity)

defaultH :: Environment -> Error -> Action
defaultH e x = do
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o


getFundRequestsH :: Action
getFundRequestsH = do
  fundRequests <- runDB (selectList [] [])
  status ok200
  json (fundRequests :: [Entity FundRequest])

postFundRequestsH :: Action
postFundRequestsH = do
  -- TODO: Refractor to use genKeySet
  (pubKey, privKey) <- liftIO genKeys
  let compressedPub@(Compressed pubKeyText) = compressed pubKey
      Hex privKeyText = getHexPrivateKey privKey
      address@(Address addressText) = getAddress compressedPub
  fundRequestRaw <- jsonData
  let either = validateFundRequest address fundRequestRaw
      keyset = KeySet addressText privKeyText pubKeyText
  case either of
    Left error -> do
      status badRequest400
      json $ object ["error" .= showError error]
    Right fundRequest -> do
      runDB (insert_ keyset)
      runDB (insert_ fundRequest)
      json fundRequest
      status ok200

-- FundRequest:  
-- Documented in BIP 0021
-- All elements should be UTF-8
-- and Percent Encoded as in RFC 3986

data FundRequestRaw = FundRequestRaw
  { labelRaw :: T.Text
  , messageRaw :: T.Text
  , amountRaw :: String }
  deriving (Generic, Show)

instance FromJSON FundRequestRaw

validateFundRequest :: Address -> FundRequestRaw -> Either Error FundRequest
validateFundRequest (Address address) fundRequestRaw@(FundRequestRaw labelR messageR amountR) =
  let
    ma :: Maybe Double
    ma = maybeRead amountR
    uri = "bitcoin:"-- ++ address
  in
    case ma of
      Nothing -> Left "Unable to parse amount"
      Just a -> Right $
        FundRequest labelR messageR a address uri
