module Avro.ToAvroSpec
( spec
)
where

import Avro.Data.Endpoint
import Avro.Data.Unions

import Avro.TestUtils              (roundtripGen)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"    :: String) #-}
{-# ANN module ("HLint: ignore Redundant flip"  :: String) #-}

spec :: Spec
spec = describe "Avro.ToAvroSpec" $ do
  describe "Should encode directly and decode via new value" $ do
    it "Unions" $ require $ property $ roundtripGen schema'Unions unionsGen
    it "Endpoint" $ require $ property $ roundtripGen schema'Endpoint endpointGen

