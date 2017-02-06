{-# Language OverloadedStrings #-}
module BloomFilter where

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
nTweak :: Tweak
nTweak = Tweak 0

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
    
  
-- s: filterSize
numberHashFunctions s n = min calculatedHashFunctions maxHashFuncs
  where calculatedHashFunctions = floor $ (s * 8) / (n * (log 2))

bloomHash :: Int -> Tweak -> ByteString -> Int -> Probability -> Int
bloomHash hashNum tweak hashData n p =
  hash `mod` filterLengthBits
  where
    hash = (fromIntegral $ murmur3 seedValue hashData)
    seedValue = seed hashNum tweak
    filterLengthBits = (filterSize n p) * 8

updateFilter :: Filter -> Int -> Filter
updateFilter (Filter filter) index = Filter $ setBit filter index

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
