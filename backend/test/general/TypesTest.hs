module General.TypesTest where

import TestUtil
import General.Types
import Data.Binary (Binary(..))
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)

networkInvertible = testCase
  "We should be able to serialize and deserialize the `Network`"
  (mapM_ (assertBool "NetworkInvertible test") testCases)
  where
    testCases = map (prop_networkInvertible . fst) networkTable

prop_networkInvertible :: Network -> Bool
prop_networkInvertible network =
  parsedNetwork == network
  where
    parsedNetwork = runGet get (runPut . put $ network)
