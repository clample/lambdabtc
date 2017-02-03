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
  , testCase) where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, suchThat, vectorOf, elements, oneof, listOf, Gen)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8

hexBS :: Int -> Gen ByteString
hexBS i = Char8.pack <$> vectorOf i hexChars
  where
    hexChars = elements ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
