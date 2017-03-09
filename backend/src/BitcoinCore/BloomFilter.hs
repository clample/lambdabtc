{-# LANGUAGE TemplateHaskell #-}
{-# Language OverloadedStrings #-}
module BitcoinCore.BloomFilter
  ( Filter(..)
  , Tweak(..)
  , NFlags(..)
  , probability
  , maxFilterBytes
  , numberHashFunctions
  , filterSize
  , pDefault
  , blankFilter
  , updateFilter
  , hardcodedTweak
  , unroll
  , roll
  , serializeFilter
  , deserializeFilter
  , filterLengthBytes
  , FilterContext(..)
  , tweak
  , nHashFunctions
  , defaultFilterWithElements
  )where

import General.Util (roll, unroll, Endian(..))

import Data.Hash.Murmur (murmur3)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Base16 (encode)
import Data.Bits (setBit)
import Control.Lens (makeLenses, (^.), over)


data Filter = Filter
  { _filterLengthBytes :: Int
  , _filterValue       :: Integer
  } deriving (Eq)

data FilterContext = FilterContext
  { _tweak :: Tweak
  , _nHashFunctions :: Int
  } deriving (Eq, Show)
  -- TODO: Include NFlags?

newtype Tweak = Tweak Int
  deriving (Show, Eq)

makeLenses ''Filter
makeLenses ''FilterContext

filterLengthBits :: Filter -> Int
filterLengthBits f = 8 * (f^.filterLengthBytes)

instance Show Filter where
  show f =
    "Filter { filterLengthBytes = " ++ show  (f^.filterLengthBytes)
    ++ " filterValue = " ++ show (f^.filterValue)
    ++ " hexEncoded = " ++ (show . encode . serializeFilter) f
    ++ " } "

newtype Probability = Probability Float
  deriving (Show, Eq)

data NFlags
  = BLOOM_UPDATE_NONE
  | BLOOP_UPDATE_ALL
  | BLOOM_UPDATE_P2PUBKEY_ONLY
  deriving (Enum, Show, Eq, Bounded)

probability :: Float -> Probability
probability p =
  if p >= 0 && p <= 1
  then Probability p
  else error $ "Can't construct a probability " ++ show p

-- For reference, see: https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki

maxFilterBytes :: Int
maxFilterBytes = 36000

maxHashFuncs :: Int
maxHashFuncs = 50

-- TODO: This should be random, not hardcoded
hardcodedTweak :: Tweak
hardcodedTweak = Tweak 0

pDefault :: Probability
pDefault = probability 0.0001

seed :: Int -> Tweak -> Word32
seed hashNum (Tweak tweak') = fromIntegral $ (hashNum * 0xFBA4C795) + tweak'

-- n: number of elements to be added to the set
-- p: probability of false positive. 1.0 is match everything, 0 is unachievable
-- returns the filter size in bytes
filterSize :: Int -> Probability -> Int
filterSize nElements (Probability p) =
  min (floor (numerator / denominator)) maxFilterBytes
  where
    numerator = (-1) * fromIntegral nElements * log p 
    denominator = (log 2 ^ 2) * 8
    
  
-- s: filter size (Bytes)
numberHashFunctions :: Int -> Int -> Int
numberHashFunctions s nElements = min calculatedHashFunctions maxHashFuncs
  where calculatedHashFunctions = floor $ (fromIntegral s * 8 * (log 2 :: Double)) / fromIntegral nElements

updateFilter :: FilterContext -> ByteString -> Filter -> Filter
updateFilter  filterContext hashData fltr=
  foldl (\fltr' updateFunc -> updateFunc fltr') fltr updateFuncs
  where
    updateFuncs = map
      (updateFilterStep filterContext hashData)
      [0..((filterContext^.nHashFunctions) - 1)]

updateFilterStep :: FilterContext -> ByteString -> Int -> Filter -> Filter
updateFilterStep filterContext hashData hashNum f =
    over filterValue (`setBit` index) f
    where
      index = bloomHash hashNum (filterContext^.tweak) hashData (filterLengthBits f)

-- Performs a single hash and returns the hash value
bloomHash :: Int -> Tweak -> ByteString -> Int -> Int
bloomHash hashNum tweak hashData fLengthBits =
  hash `mod` fLengthBits
  where
    hash = fromIntegral $ murmur3 seedValue hashData
    seedValue = seed hashNum tweak      

defaultFilterWithElements :: [ByteString] -> (Filter, FilterContext)
defaultFilterWithElements elements = (filter', context)
  where
    filter' = foldr (updateFilter context) blank elements
    (blank, context) = blankFilter nElements pDefault
    nElements = max 10 (length elements)
      -- in case there are no elements to add to the filter
      -- we still use nElements 10, so the filter isn't blank

blankFilter :: Int -> Probability -> (Filter, FilterContext)
blankFilter nElements p = (bloomFilter, context)
  where bloomFilter = Filter
          { _filterLengthBytes = size
          , _filterValue =  0}
        context = FilterContext
          { _nHashFunctions = nHashFunctions'
          , _tweak = tweak' }
        size = filterSize nElements p
        nHashFunctions' = numberHashFunctions size nElements
        tweak' = hardcodedTweak

serializeFilter :: Filter -> ByteString
serializeFilter f = (unroll LE  (f^.filterValue)) `B.append` paddingNullBytes
  where filterBase = unroll LE  (f^.filterValue)
        paddingNullBytes = B.replicate ((f^.filterLengthBytes) - B.length filterBase) 0

deserializeFilter :: ByteString -> Filter
deserializeFilter bs = Filter
  {_filterLengthBytes = fLength
  , _filterValue = fValue
  }
  where fLength = B.length bs
        fValue = roll LE bs
