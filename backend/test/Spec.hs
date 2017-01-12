import Prelude hiding (length)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

import Keys
import Crypto.PubKey.ECC.ECDSA (PublicKey(..))
import Crypto.PubKey.ECC.Types (Curve, getCurveByName, Point(..), CurveName(SEC_p256k1))
import Data.ByteString (length)

main :: IO ()
main = defaultMain tests

tests =
  [
    testGroup "Keys tests" [
      testCase "Uncompressed public key length"
        $ uncompressedPubKeyLength $ uncompressed testKey
      ]
  ]

testKey :: PublicKey
testKey = PublicKey
  (getCurveByName SEC_p256k1)
  (Point
    55066263022277343669578718895168534326250603453777594175500187360389116729240
    32670510020758816978083085130507043184471273380659243275938904335757337482424)

uncompressedPubKeyLength :: PublicKeyRep -> Assertion
uncompressedPubKeyLength (Uncompressed key) = assertEqual
  "Uncompressed key should have 65 bytes"
  65
  $ length key

uncompressedPubKey :: Assertion
uncompressedPubKey = undefined
