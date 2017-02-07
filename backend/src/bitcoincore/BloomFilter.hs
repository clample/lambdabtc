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
  )where

import Data.Hash.Murmur (murmur3)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Base16 (encode, decode)
import Data.Bits (setBit, shiftR, shiftL, (.|.))
import Data.List (unfoldr)


-- TODO: rewrite with lenses
data Filter = Filter
  { filterLengthBytes :: Int
  , filterValue       :: Integer
  } deriving (Eq)

filterLengthBits :: Filter -> Int
filterLengthBits f = 8 * filterLengthBytes f

instance Show Filter where
  show f =
    "Filter { filterLengthBytes = " ++ (show . filterLengthBytes) f
    ++ " filterValue = " ++ (show . filterValue) f
    ++ " hexEncoded = " ++ (show . encode . serializeFilter) f
    ++ " } "

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
    
  
-- s: filter size (Bytes)
numberHashFunctions :: Int -> Int -> Int
numberHashFunctions s n = min calculatedHashFunctions maxHashFuncs
  where calculatedHashFunctions = floor $ ((fromIntegral s) * 8 * log 2) / (fromIntegral n)

updateFilter :: Int -> Tweak -> ByteString -> Filter -> Filter
updateFilter numberHashes tweak hashData filter =
  foldl (\f updateFunc -> updateFunc f) filter updateFuncs
  where
    updateFuncs = map
      (\hashNum -> updateFilterStep hashNum tweak hashData)
      [0..(numberHashes - 1)]

updateFilterStep :: Int -> Tweak -> ByteString -> Filter -> Filter
updateFilterStep hashNum tweak hashData f =
    f { filterValue = setBit  (filterValue f) index }
    where
      index = bloomHash hashNum tweak hashData (filterLengthBits f)

bloomHash :: Int -> Tweak -> ByteString -> Int -> Int
bloomHash hashNum tweak hashData fLengthBits =
  hash `mod` fLengthBits
  where
    hash = (fromIntegral $ murmur3 seedValue hashData)
    seedValue = seed hashNum tweak      

blankFilter :: Int -> Probability -> Filter
blankFilter n p = Filter { filterLengthBytes = (filterSize n p), filterValue =  0 }


-- Taken from src of Data.Binary
-- http://hackage.haskell.org/package/binary-0.4.1/docs/src/Data-Binary.html#Binary
unroll :: Integer -> ByteString
unroll = B.pack . unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: ByteString -> Integer
roll   = foldr unstep 0 . B.unpack
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

serializeFilter :: Filter -> ByteString
serializeFilter f = (unroll . filterValue $ f) `B.append` paddingNullBytes
  where filterBase = unroll . filterValue $ f
        paddingNullBytes = B.replicate (filterLengthBytes f - B.length filterBase) 0

deserializeFilter :: ByteString -> Filter
deserializeFilter bs = Filter {filterLengthBytes = fLength, filterValue = fValue}
  where fLength = B.length bs
        fValue = roll bs

{--
setFilterTest :: IO ()
setFilterTest = do
  let
    blank = blankFilter 1 pDefault
    s = filterSize 1 pDefault
    nHashFuncs = numberHashFunctions s 1
    txId = fst . decode $ "019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65"
    filter' = updateFilter nHashFuncs hardcodedTweak txId blank
  putStrLn $ "Blank: " ++ show blank
  putStrLn $ "Filter size " ++ show s
  putStrLn $ "number of hash functions " ++ show nHashFuncs
  putStrLn $ "Final filter " ++ show filter'
--}
