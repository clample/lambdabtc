module Protocol.Server where

import qualified Data.ByteString.Char8 as Char8
import Protocol.Parser (parseVersionMessage)
import Messages (VersionMessage(..), Network(..), getAddr, showVersionMessage, Addr(..), showMessage, MessageContext(..), Message(..), MessageBody(..))
import Text.Megaparsec (parseMaybe)
import Network.Socket (connect
                      , socket
                      , Family(..)
                      , SocketType(..)
                      , defaultProtocol
                      , connect
                      , SockAddr(..)
                      , tupleToHostAddress
                      , getAddrInfo
                      , AddrInfo(..)
                      , iNADDR_ANY
                      , bind
                      , defaultHints
                      , AddrInfoFlag(..)
                      , setSocketOption
                      , SocketOption(..)
                      , hostAddressToTuple)
import Network.Socket.ByteString (send, recv )
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.ByteString.Base16 (decode, encode)

-- Find testnet hosts with `nslookup testnet-seed.bitcoin.petertodd.org`

connectTestnet = do
  addrInfo <- (head) <$> getAddrInfo Nothing (Just "testnet-seed.bitcoin.petertodd.org") (Just "18333")
  peerSocket <- socket (addrFamily addrInfo) Stream defaultProtocol
  setSocketOption peerSocket KeepAlive 1
  connect peerSocket (addrAddress addrInfo)
  putStrLn "Great Job, we connected"
  time <- getPOSIXTime
  let message = showMessage $ Message (Version $ VersionMessage 60002 100 10 (getAddr $ addrAddress addrInfo) senderAddr) (MessageContext TestNet3 time)
      senderAddr = Addr (207, 251, 103, 46 ) 18333
  send peerSocket $ fst . decode $ message
  putStrLn . Char8.unpack $ message
  bs <- recv peerSocket 300
  let response = Char8.unpack . encode $ bs
  putStrLn $ "Recieved message: " ++ response
  putStrLn . show $ parseMaybe parseVersionMessage response
