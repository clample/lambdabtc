{-# LANGUAGE OverloadedStrings #-}

module TestUtil
  (hexBS
  , Gen
  , listOf
  , oneof
  , elements
  , vectorOf
  , suchThat
  , choose
  , Arbitrary(..)
  , testProperty
  , Assertion
  , assertEqual
  , testCase
  , arbitraryBoundedEnum
  , vector
  , buildTest
  , assertBool
  , testPublicKeyRep
  , buildTestBracketed
  , Test
  ) where

import BitcoinCore.Keys

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitraryBoundedEnum, vector)
import Test.QuickCheck.Gen (choose, suchThat, vectorOf, elements, oneof, listOf, Gen)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.API (buildTest, buildTestBracketed, Test)
import Test.HUnit (assertEqual, Assertion, assertBool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as BIN
import Data.ByteString.Base16 (decode)

hexBS :: Int -> Gen ByteString
hexBS i = Char8.pack <$> vectorOf i hexChars
  where
    hexChars = elements ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']

-- This key rep is from https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
testPublicKeyRep :: PublicKeyRep
testPublicKeyRep = BIN.decode lazyBS
  where
    lazyBS = BL.fromChunks [bs]
    bs = fst . decode $ 
      "0450863AD64A87AE8A2FE83C1AF1A8403CB53F53E486D8511DAD8A04887E5B23522CD470243453A299FA9E77237716103ABC11A1DF38855ED6F2EE187E9C582BA6"
