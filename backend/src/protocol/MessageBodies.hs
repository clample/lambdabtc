{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Protocol.MessageBodies where

import Protocol.Network (Addr, putServices, putAddr, getAddr)
import General.Types (HasRelay(..), HasTime(..), HasLastBlock(..), HasVersion(..))
import General.Util (VarInt(..))
import BitcoinCore.Inventory (InventoryVector(..))
import BitcoinCore.BlockHeaders (BlockHash(..), BlockHeader(..))
import BitcoinCore.BloomFilter (Tweak(..), Filter(..), NFlags(..), serializeFilter, deserializeFilter)

import Data.Time.Clock.POSIX (POSIXTime)
import Control.Lens (makeLenses, makeFields, (^.))
import Data.Binary (Binary(..))
import Data.Binary.Get (Get, getWord32le, getWord64be, getWord64le, getWord8, getByteString)
import Data.Binary.Put (Put, putWord32le, putWord64le, putWord64be, putWord8, putByteString)
import Data.ByteString (ByteString)
import Control.Monad (replicateM)
import Foreign.Marshal.Utils (toBool)

----------------------------------
data VersionMessage = VersionMessage
    { _versionMessageVersion    :: Int
    , _nonce   :: Integer -- Nonce can be 8 bytes -> use integer 
    , _versionMessageLastBlock :: Integer
    , _senderAddr :: Addr
    , _peerAddr   :: Addr
    , _versionMessageRelay      :: Bool
    , _versionMessageTime  :: POSIXTime
    } deriving (Show, Eq)
makeLenses ''VersionMessage

instance HasLastBlock VersionMessage where
  lastBlock = versionMessageLastBlock

instance HasRelay VersionMessage where
  relay = versionMessageRelay

instance HasTime VersionMessage where
  time = versionMessageTime

instance HasVersion VersionMessage where
  version = versionMessageVersion

instance Binary VersionMessage where
  put = putVersionMessage
  get = getVersionMessage

putVersionMessage :: VersionMessage -> Put
putVersionMessage versionMessage = do
  putWord32le . fromIntegral $ versionMessage^.version
  putServices
  putWord64le . floor $ versionMessage^.time
  putAddr (versionMessage^.peerAddr)
  putAddr (versionMessage^.senderAddr)
  putWord64be . fromIntegral $ (versionMessage^.nonce)
  putWord8 0
  putWord32le . fromIntegral $ (versionMessage^.lastBlock)
  put (versionMessage^.relay)

getVersionMessage :: Get VersionMessage
getVersionMessage = do
  version     <- fromIntegral <$> getWord32le
  services    <- getWord64be
  timestamp   <- fromIntegral <$> getWord64le
  peer        <- getAddr
  sender      <- getAddr
  nonce       <- fromIntegral <$> getWord64be
  userAgent   <- getPayload
  startHeight <- fromIntegral <$> getWord32le
  relay       <- toBool <$> getWord8 
  return
    (VersionMessage version nonce startHeight sender peer relay timestamp)
------------------------------------

getPayload :: Get ByteString
getPayload = do
  length <- fromIntegral <$> getWord8
  getByteString length

data VerackMessage = VerackMessage
  deriving (Show, Eq)

data AddrMessage = AddrMessage
  deriving (Show, Eq)

data TxMessage = TxMessage
  deriving (Show, Eq)
  

data RejectMessage = RejectMessage
  deriving (Show, Eq)

data PingMessage = PingMessage
  deriving (Show, Eq)

data PongMessage = PongMessage
  deriving (Show, Eq)

----------------------------
data InvMessage = InvMessage
  { _invVectors :: [InventoryVector]}
  deriving (Show, Eq)

makeLenses ''InvMessage

instance Binary InvMessage where
  put = putInvMessage 
  get = getInvMessage

putInvMessage :: InvMessage -> Put
putInvMessage message = do
  put . VarInt . length $ message^.invVectors
  mapM_ put (message^.invVectors)

getInvMessage :: Get InvMessage
getInvMessage = do
   VarInt count <- get
   inventoryVectors <- replicateM count get
   return $ (InvMessage inventoryVectors)

---------------------------
data GetDataMessage = GetDataMessage
  deriving (Show, Eq)

data NotFoundMessage = NotFoundMessage
  deriving (Show, Eq)

data GetBlocksMessage = GetBlocksMessage
  deriving (Show, Eq)

-------------------------------
data GetHeadersMessage = GetHeadersMessage
    { _getHeadersMessageVersion            :: Int
    , _blockLocatorHashes :: [BlockHash]
    , _hashStop           :: BlockHash}
  deriving (Show, Eq)

makeLenses ''GetHeadersMessage

instance HasVersion GetHeadersMessage where
  version = getHeadersMessageVersion

instance Binary GetHeadersMessage where
  put = putGetHeadersMessage
  get = getGetHeadersMessage

putGetHeadersMessage :: GetHeadersMessage -> Put
putGetHeadersMessage message = do
  putWord32le . fromIntegral $ (message^.version)
  put . VarInt . length $ (message^.blockLocatorHashes)
  mapM_ put (message^.blockLocatorHashes)
  put (message^.hashStop)

getGetHeadersMessage :: Get GetHeadersMessage
getGetHeadersMessage = do
  version'            <- fromIntegral <$> getWord32le
  VarInt nHashes     <- get
  blockLocatorHashes' <- replicateM nHashes get
  hashStop'           <- get
  return
    (GetHeadersMessage version' blockLocatorHashes' hashStop')
--------------------------------

data BlockMessage = BlockMessage
  deriving (Show, Eq)

------------------------
data HeadersMessage = HeadersMessage
  { _blockHeaders :: [BlockHeader]}
  deriving (Show, Eq)

makeLenses ''HeadersMessage

instance Binary HeadersMessage where
  put = putHeadersMessage
  get = getHeadersMessage

getHeadersMessage :: Get HeadersMessage
getHeadersMessage = do
  VarInt nHeaders <- get
  blockHeaders'    <- replicateM nHeaders get
  return $ HeadersMessage blockHeaders'

putHeadersMessage :: HeadersMessage -> Put
putHeadersMessage message = do
  put . VarInt . length $ (message^.blockHeaders)
  mapM_ put (message^.blockHeaders)
-------------------------

data GetAddrMessage = GetAddrMessage
  deriving (Show, Eq)

data MempoolMessage = MempoolMessage
  deriving (Show, Eq)

data CheckorderMessage = CheckorderMessage
  deriving (Show, Eq)

data SubmitorderMessage = SubmitorderMessage
  deriving (Show, Eq)

------------------------
data FilterloadMessage = FilterloadMessage
    { _filterloadMessageBloomFilter :: Filter
    , _filterloadMessageHashFuncs :: Int
    , _filterloadMessageNTweak :: Tweak
    , _filterloadMessageNFlags :: NFlags
    }
  deriving (Show, Eq)

makeFields ''FilterloadMessage

instance Binary FilterloadMessage where
  put = putFilterloadMessage
  get = getFilterloadMessage

putFilterloadMessage :: FilterloadMessage -> Put
putFilterloadMessage message = do
  put . VarInt . filterLengthBytes $ (message^.bloomFilter)
  putByteString . serializeFilter $ (message^.bloomFilter)
  putWord32le . fromIntegral $ (message^.hashFuncs)
  let Tweak t = message^.nTweak
  putWord32le . fromIntegral $ t
    -- TODO: It's not clear if nTweak should be litle or big endian
    --       This won't be a problem when nTweak is 0, but it may cause bugs later
  putWord8 . fromIntegral . fromEnum $ (message^.nFlags)

getFilterloadMessage :: Get FilterloadMessage
getFilterloadMessage = do
  VarInt lengthFilter <- get
  filter <- deserializeFilter <$> getByteString lengthFilter
  nHashFuncs <- fromIntegral <$> getWord32le
  nTweak     <- Tweak . fromIntegral <$> getWord32le
  nFlags <- toEnum . fromIntegral <$> getWord8
  return
    (FilterloadMessage filter nHashFuncs nTweak nFlags)

  
------------------------
  

data FilteraddMessage = FilteraddMessage
  deriving (Show, Eq)

data FilterclearMessage = FilterclearMessage
  deriving (Show, Eq)

data MerkleblockMessage = MerkleblockMessage
  deriving (Show, Eq)

data SendheadersMessage = SendheadersMessage
  deriving (Show, Eq)

data FeefilterMessage = FeefilterMessage
  deriving (Show, Eq)

data SendcmpctMessage = SendcmpctMessage
  deriving (Show, Eq)

data GetblocktxnMessage = GetblocktxnMessage
  deriving (Show, Eq)

data BlocktxnMessage = BlocktxnMessage
  deriving (Show, Eq)

data UnknownMessage = UnknownMessage
  deriving (Show, Eq)

data CmpctblockMessage = CmpctblockMessage
  deriving (Show, Eq)
