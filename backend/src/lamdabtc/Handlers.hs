{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module LamdaBTC.Handlers
  ( defaultH
  , postFundRequestsH
  , getFundRequestsH
  , postTransactionsH
  ) where

import BitcoinCore.Keys
import BitcoinCore.Transaction.Transactions ( Transaction(..)
                                            , TxInput(..)
                                            , TxOutput(..)
                                            , UTXO(..)
                                            , Value(..)
                                            , TxVersion(..)
                                            , TxIndex(..)
                                            , defaultVersion
                                            , signedTransaction)
import qualified BitcoinCore.Transaction.Transactions as TX
import BitcoinCore.Transaction.Script (payToPubkeyHash, getScript, Script(..), ScriptComponent(..))
import General.InternalMessaging (InternalMessage(..))

import General.Persistence
import General.Config
import General.Types (HasNetwork(..), Network(..))
import General.Util (maybeRead, decodeBase58Check, Payload(..))
import Crypto.PubKey.ECC.ECDSA (PrivateKey(..), PublicKey(..))

import Network.HTTP.Types.Status (internalServerError500, ok200, badRequest400)
import Data.Aeson ( object
                  , (.=)
                  , Value (Null)
                  , FromJSON)
import Web.Scotty.Trans (status, showError, json, jsonData, ActionT)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T 
import Database.Persist.Sql (insert_, selectList)
import Database.Persist (Entity(..), get)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.), makeLenses)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decode)
import Control.Concurrent.STM.TBMChan (writeTBMChan)
import GHC.Conc (atomically)
import Data.Binary.Get (runGet)
import qualified Data.Binary as Binary

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
  config <- lift ask
  -- TODO: Refractor to use genKeySet
  (pubKey, privKey) <- liftIO genKeys
  let WIF privKeyTxt = getWIFPrivateKey privKey
      address@(Address addressText) =
        getAddress (PublicKeyRep Compressed pubKey) (config^.network)
  fundRequestRaw <- jsonData
  let eitherFundRequest = validateFundRequest address fundRequestRaw
      keyset = KeySet addressText privKeyTxt
  case eitherFundRequest of
    Left errorMessage -> do
      status badRequest400
      json $ object ["error" .= showError errorMessage]
    Right fundRequest -> do
      runDB (insert_ keyset)
      runDB (insert_ fundRequest)
      json fundRequest
      status ok200

genKeySet :: Network -> IO KeySet
genKeySet network = do
  (pubKey, privKey) <- liftIO genKeys
  let WIF privKeyText = getWIFPrivateKey privKey
      (Address addressText) = getAddress (PublicKeyRep Compressed pubKey) network
  return (KeySet addressText privKeyText)

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
validateFundRequest (Address address) (FundRequestRaw labelR messageR amountR) =
  let
    ma :: Maybe Double
    ma = maybeRead amountR
    uri = "bitcoin:"
  in
    case ma of
      Nothing -> Left "Unable to parse amount"
      Just a -> Right $
        FundRequest labelR messageR a address uri

data TransactionRaw = TransactionRaw
  { recieverAddress :: String
  , transactionAmountRaw :: String }
  deriving (Generic, Show)

instance FromJSON TransactionRaw

postTransactionsH :: Action
postTransactionsH = do
  transactionRaw <- jsonData
  transaction <- buildTransaction transactionRaw
  config <- lift ask
  liftIO . atomically $ writeTBMChan (config^.appChan) (SendTX transaction)
  status ok200

buildTransaction :: TransactionRaw -> ActionT Error ConfigM Transaction
buildTransaction txRaw = do
  let mVal = buildValue (transactionAmountRaw txRaw)
      val = fromJust mVal
      mAddress = buildAddress (recieverAddress txRaw)
      address = fromJust mAddress
      outputs' = [TxOutput
                  { _value = val
                  , _outputScript = payToPubkeyHash . addressToPubKeyHash $ address}]
  (utxo', keys', oldInputScript) <- getUTXOAndKeys 
  return $ signedTransaction utxo' oldInputScript keys' outputs'

-- TODO: Improve getting UTXO's
--       We currently only get 1 hardcoded UTXO
getUTXOAndKeys :: ActionT Error ConfigM (UTXO, (PublicKey, PrivateKey), Script)
getUTXOAndKeys = do
  mpUTXO <- runDB $ get (PersistentUTXOKey 1)
  let putxo = case mpUTXO of
                Nothing -> error $ "There are no utxo's in the db"
                Just pUTXO -> pUTXO
      utxo = UTXO {_outTxHash = TX.TxHash . persistentUTXOOutTxHash $ putxo
                  , _outIndex = TxIndex . persistentUTXOOutIndex $ putxo}
      scriptBS = persistentUTXOScript putxo
      oldInputScript = runGet (getScript . BS.length $ scriptBS) (BL.fromChunks [scriptBS]) 
  mpKeySet <- runDB $ get (KeySetKey . fromIntegral . persistentUTXOKeySetId $ putxo)
  let (KeySet address privKeyT) = case mpKeySet of
                                    Nothing -> error $ "unable to find keyset with id "
                                      ++ (show . persistentUTXOKeySetId) putxo
                                    Just pKeySet -> pKeySet
      privKey = getPrivateKeyFromWIF . WIF $ privKeyT
      keySet = (getPubKey privKey, privKey)
  return (utxo, keySet, oldInputScript)
  
buildValue :: String -> Maybe TX.Value
buildValue str = TX.Satoshis <$> maybeRead str

buildAddress :: String -> Maybe Address
buildAddress = Just . Address . T.pack
