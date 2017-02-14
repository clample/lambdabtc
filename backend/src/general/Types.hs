{-# Language OverloadedStrings #-}
module General.Types where

import General.Util (readFromTable)

import Control.Lens (Lens')
import Data.ByteString (ByteString)
import Data.Binary.Get (Get, getByteString)
import Data.ByteString.Base16 (encode, decode)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (POSIXTime)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

class HasNetwork t where
  network :: Lens' t Network

data Network = TestNet3 | MainNet
  deriving (Show, Eq)

instance Arbitrary Network where
  arbitrary = elements [TestNet3, MainNet]


networkTable :: [(Network, ByteString)]
networkTable =
  [ (TestNet3, "0B110907")
  , (MainNet,  "F9BEB4D9")]


-- TODO: Is print network ever called? Otherwise, just move it under getNetwork
-- TODO: Can we come up with better names for these network functions
--       It's not clear from the name which function does what
printNetwork :: Network -> ByteString
printNetwork = fromJust . flip lookup networkTable

getNetwork' :: Network -> ByteString
getNetwork' = fst . decode . printNetwork

readNetwork :: ByteString -> Maybe Network
readNetwork = readFromTable networkTable

getNetwork :: Get Network
getNetwork = do
  mNetwork <- readNetwork . encode <$> getByteString 4
  case mNetwork of
    Just network' -> return network'
    Nothing -> fail "Unable to parse network"

class HasRelay t where
  relay :: Lens' t Bool

class HasTime t where
  time :: Lens' t POSIXTime

class HasLastBlock t where
  lastBlock :: Lens' t Integer

class HasVersion t where
  version :: Lens' t Int

