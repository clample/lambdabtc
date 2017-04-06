{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module LamdaBTC.Handlers
  ( defaultH
  , postFundRequestsH
  , getFundRequestsH
  , postTransactionsH
  , handleIncomingFunds
  , getStatusH
  , getUTXOsH
  ) where

import BitcoinCore.Keys
import BitcoinCore.Transaction.Transactions
  ( Transaction(..)
  , TxOutput(..)
  , UTXO(..)
  , TxIndex(..)
  , signedTransaction
  )
import qualified BitcoinCore.Transaction.Transactions as TX
import BitcoinCore.Transaction.Script (payToPubkeyHash, getScript, Script(..))
import General.InternalMessaging (InternalMessage(..))
import General.Persistence
import General.Config
import General.Types (HasNetwork(..), Network(..), HasPool(..))
import General.Util (maybeRead)
import General.Hash (Hash(..))
import Protocol.Persistence
  ( getUnspentUTXOs
  , setUtxoSpent
  )

import Crypto.PubKey.ECC.ECDSA (PrivateKey(..), PublicKey(..))
import qualified Network.HTTP.Types.Status as Status
import Data.Aeson
  ( object
  , (.=)
  , Value (Null)
  , FromJSON
  )
import Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as ScottyT
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T 
import Database.Persist.Sql (insert_, selectList)
import Database.Persist (Entity(..), get)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Control.Concurrent.STM.TBMChan (writeTBMChan)
import GHC.Conc (atomically)
import Data.Binary.Get (runGet)
import Network.WebSockets (Connection, sendTextData)
import qualified Database.Persist.Sql as DB

defaultH :: Environment -> Error -> Action
defaultH e x = do
  ScottyT.status Status.internalServerError500
  let o = case e of
        Development -> object ["error" .= ScottyT.showError x]
        Production -> Null
        Test -> object ["error" .= ScottyT.showError x]
  ScottyT.json o

getFundRequestsH :: Action
getFundRequestsH = do
  fundRequests <- runDB (selectList [] [])
  ScottyT.status Status.ok200
  ScottyT.json (fundRequests :: [Entity FundRequest])

postFundRequestsH :: Action
postFundRequestsH = do
  config <- lift ask
  -- TODO: Refractor to use genKeySet
  (pubKey, privKey) <- liftIO genKeys
  let WIF privKeyTxt = getWIFPrivateKey privKey
      address =
        getAddress (PublicKeyRep Compressed pubKey) (config^.network)
  fundRequestRaw <- ScottyT.jsonData
  let eitherFundRequest = validateFundRequest address fundRequestRaw
      keyset = KeySet (address^.addrTxt) privKeyTxt
  case eitherFundRequest of
    Left errorMessage -> do
      ScottyT.status Status.badRequest400
      ScottyT.json $ object ["error" .= ScottyT.showError errorMessage]
    Right fundRequest -> do
      runDB (insert_ keyset)
      runDB (insert_ fundRequest)
      sendInternalMessage $ AddAddress address
      ScottyT.json fundRequest
      ScottyT.status Status.ok200

getUTXOsH :: Action
getUTXOsH = do
  config <- lift ask
  utxos <- runDB (selectList [] [])
  ScottyT.status Status.ok200
  ScottyT.json $ map displayUTXO (utxos :: [Entity PersistentUTXO])

genKeySet :: Network -> IO KeySet
genKeySet network' = do
  (pubKey, privKey) <- liftIO genKeys
  let WIF privKeyText = getWIFPrivateKey privKey
      address = getAddress (PublicKeyRep Compressed pubKey) network'
  return $ KeySet (address^.addrTxt) privKeyText

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
validateFundRequest address (FundRequestRaw labelR messageR amountR) =
  let
    ma :: Maybe Double
    ma = maybeRead amountR
    uri = "bitcoin:"
  in
    case ma of
      Nothing -> Left "Unable to parse amount"
      Just a -> Right $
        FundRequest labelR messageR a (address^.addrTxt) uri

data TransactionRaw = TransactionRaw
  { recieverAddress :: String
  , transactionAmountRaw :: String }
  deriving (Generic, Show)

instance FromJSON TransactionRaw

postTransactionsH :: Action
postTransactionsH = do
  transactionRaw <- ScottyT.jsonData
  transaction <- buildTransaction transactionRaw
  sendInternalMessage (SendTX transaction)
  ScottyT.status Status.ok200

buildTransaction :: TransactionRaw -> ActionT Error ConfigM Transaction
buildTransaction txRaw = do
  let mVal = buildValue (transactionAmountRaw txRaw)
      val = fromJust mVal
      mAddress = buildAddress (recieverAddress txRaw)
      address = fromJust mAddress
      outputs' = [TxOutput
                  { _value = val
                  , _outputScript = payToPubkeyHash . addressToPubKeyHash $ address}]
  (utxo', keys', oldInputScript) <- getUTXOAndKeys val
  return $ signedTransaction utxo' oldInputScript keys' outputs'

getUTXOAndKeys :: TX.Value
               -> ActionT Error ConfigM (UTXO, (PublicKey, PrivateKey), Script)
getUTXOAndKeys (TX.Satoshis val) = do
  config <- lift ask
  putxos <- liftIO $ getUnspentUTXOs $ config^.pool
  let candidateUtxos = filter (\(DB.Entity _ putxo') -> (persistentUTXOValue putxo') > val) putxos
      (DB.Entity idPutxo putxo) = case candidateUtxos of
                [] -> error "Unable to find utxo with enough value"
                (putxo':putxos') -> putxo'

      -- TODO: Create a function `decodeUtxo :: PersistentUTXO -> UTXO`
      utxo = UTXO {_outTxHash = Hash . persistentUTXOOutTxHash $ putxo
                  , _outIndex = TxIndex . persistentUTXOOutIndex $ putxo}
             
      scriptBS = persistentUTXOScript putxo
      oldInputScript = runGet (getScript . BS.length $ scriptBS) (BL.fromChunks [scriptBS]) 
  mpKeySet <- runDB $ get (KeySetKey . fromIntegral . persistentUTXOKeySetId $ putxo)
  let (KeySet address privKeyT) = fromMaybe
        (error $ "unable to find keyset with id "
          ++ (show . persistentUTXOKeySetId) putxo)
        mpKeySet
      privKey = getPrivateKeyFromWIF . WIF $ privKeyT
      keySet = (getPubKey privKey, privKey)
  liftIO $ setUtxoSpent (config^.pool) idPutxo
  return (utxo, keySet, oldInputScript)
  
buildValue :: String -> Maybe TX.Value
buildValue str = TX.Satoshis <$> maybeRead str

buildAddress :: String -> Maybe Address
buildAddress = Just . Address . T.pack

sendInternalMessage :: InternalMessage -> Action
sendInternalMessage internalMessage = do
  config <- lift ask
  liftIO . atomically $ writeTBMChan (config^.appChan) internalMessage

handleIncomingFunds :: Connection -> TX.Value -> IO ()
handleIncomingFunds connection val = do
  let valText = T.pack . show $ val
  sendTextData connection valText
  
getStatusH :: Action
getStatusH =
  ScottyT.status Status.ok200
