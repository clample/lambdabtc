{-# Language OverloadedStrings #-}
module General.Types where

import General.Util (Addr(..))

import Control.Lens (Lens')
import Data.Binary (Binary(..), Word32)
import Data.Binary.Get (Get, getWord32be)
import Data.Binary.Put (Put, putWord32be)
import Data.Time.Clock.POSIX (POSIXTime)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)
import Database.Persist.Sql (ConnectionPool)
import Data.Tuple (swap)

class HasNetwork t where
  network :: Lens' t Network

data Network = TestNet3 | MainNet
  deriving (Show, Eq)

instance Arbitrary Network where
  arbitrary = elements [TestNet3, MainNet]

instance Binary Network where
  put = putNetwork
  get = getNetwork

putNetwork :: Network -> Put
putNetwork network =
  case lookup network networkTable of
    Just w32 -> putWord32be w32
    Nothing -> error $
      "Unable to find network "
      ++ show network
      ++ " in network table."

getNetwork :: Get Network
getNetwork = do
  w32 <- getWord32be
  let table' = map swap networkTable
  case lookup w32 table' of
    Just network -> return network
    Nothing -> error $
      "Unable to find network corresponding to code "
      ++ show w32
      ++ " in network table"


networkTable :: [(Network, Word32)]
networkTable =
  [ (TestNet3, 0x0B110907)
  , (MainNet,  0xF9BEB4D9)]

class HasRelay t where
  relay :: Lens' t Bool

class HasTime t where
  time :: Lens' t POSIXTime

class HasLastBlock t where
  lastBlock :: Lens' t Integer

class HasVersion t where
  version :: Lens' t Int

class HasPeerAddr t where
  peerAddr :: Lens' t Addr

class HasPool t where
  pool :: Lens' t ConnectionPool
