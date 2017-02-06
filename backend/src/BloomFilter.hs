{-# Language OverloadedStrings #-}
module BloomFilter
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
  )where

import Data.Hash.Murmur (murmur3)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Base16 (encode, decode)
import Data.Bits (setBit)
import Data.Bits.ByteString


newtype Filter = Filter ByteString
  deriving (Show, Eq)

newtype Tweak = Tweak Int
  deriving (Show, Eq)

newtype Probability = Probability Float
  deriving (Show, Eq)

data NFlags
  = BLOOM_UPDATE_NONE
  | BLOOP_UPDATE_ALL
  | BLOOM_UPDATE_P2PUBKEY_ONLY
  deriving (Enum, Show, Eq, Bounded)

probability :: Float -> Probability
probability p =
  if (p >= 0 && p <= 1)
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
seed hashNum (Tweak tweak) = fromIntegral $ (hashNum * 0xFBA4C795) + tweak

-- n: number of elements to be added to the set
-- p: probability of false positive. 1.0 is match everything, 0 is unachievable
-- returns the filter size in bytes
filterSize :: Int -> Probability -> Int
filterSize n (Probability p) =
  min (floor (numerator / denominator)) maxFilterBytes
  where
    numerator = (-1) * (fromIntegral n) * (log p) 
    denominator = ((log 2) ^ 2) * 8
    
  
-- s: filterSize (Bytes)
numberHashFunctions :: Int -> Int -> Int
numberHashFunctions s n = min calculatedHashFunctions maxHashFuncs
  where calculatedHashFunctions = floor $ ((fromIntegral s) * 8) / ((fromIntegral n) * (log 2))

updateFilter :: Int -> Tweak -> ByteString -> Filter -> Filter
updateFilter numberHashes tweak hashData filter =
  foldl (\f updateFunc -> updateFunc f) filter updateFuncs
  where
    updateFuncs = map
      (\hashNum -> updateFilterStep hashNum tweak hashData)
      [0..(numberHashes - 1)]

updateFilterStep :: Int -> Tweak -> ByteString -> Filter -> Filter
updateFilterStep hashNum tweak hashData (Filter filter) =
    Filter $ setBit filter index
    where
      filterLengthBits = (B.length filter) * 8
      index = bloomHash hashNum tweak hashData filterLengthBits

bloomHash :: Int -> Tweak -> ByteString -> Int -> Int
bloomHash hashNum tweak hashData filterLengthBits =
  hash `mod` filterLengthBits
  where
    hash = (fromIntegral $ murmur3 seedValue hashData)
    seedValue = seed hashNum tweak      

blankFilter :: Int -> Probability -> Filter
blankFilter n p = Filter . B.pack . replicate (filterSize n p) $ fromIntegral 0

{--
hashTest :: IO ()
hashTest = do
  mapM_ runHash [0..10]
  where
    runHash hashNum = do
      putStrLn $ "hashNum " ++ show hashNum ++ " nIndex: " ++ show (bloomHash hashNum tweak hashData n pDefault)
    tweak = Tweak 0
    n = 1
    hashData = fst . decode $  "019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65"
--} 
