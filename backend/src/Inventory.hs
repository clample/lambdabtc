module Inventory where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode)
import Data.Binary (Binary(..))
import Data.Binary.Put (Put, putWord32le, putByteString)
import Data.Binary.Get (Get(..), getWord32le, getByteString)

data InventoryVector =
  InventoryVector ObjectType ObjectHash
  deriving (Show, Eq)

instance Binary InventoryVector where
  put = putInventoryVector
  get = getInventoryVector

data ObjectType
  = ERROR
  | MSG_TX
  | MSG_BLOCK
  | MSG_FILTERED_BLOCK
  | MSG_CMPCT_BLOCK
  deriving (Show, Eq, Enum, Bounded)

-- Should be 32 bytes
newtype ObjectHash = ObjectHash ByteString
  deriving (Eq)

objectHash :: ByteString -> ObjectHash
objectHash bs =
  if BS.length bs == 32
  then ObjectHash bs
  else error $ "ObjectHashes should have length 32: "
               ++ (show . encode $ bs)

instance Show ObjectHash where
  show (ObjectHash bs) = "ObjectHash " ++ (show . encode $ bs)


putInventoryVector :: InventoryVector -> Put
putInventoryVector (InventoryVector objType (ObjectHash objHash)) = do
  putWord32le . fromIntegral . fromEnum $ objType
  putByteString objHash

getInventoryVector :: Get InventoryVector
getInventoryVector = do
  objType <- (toEnum . fromIntegral) <$> getWord32le
  objHash <- ObjectHash <$> getByteString 32
  return $ InventoryVector objType objHash
