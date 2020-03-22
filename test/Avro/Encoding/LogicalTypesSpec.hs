module Avro.Encoding.LogicalTypesSpec
where

import Avro.Data.Logical
import Data.Avro                   (decodeValueWithSchema, encodeValue)
import Data.Avro.Schema.ReadSchema (fromSchema)

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"    :: String) #-}
{-# ANN module ("HLint: ignore Redundant flip"  :: String) #-}

spec :: Spec
spec = describe "Avro.Encoding.LogicalTypesSpec" $ do
  describe "Round-tripping" $ do
    it "shoule encode with ToAvro and decode with FromAvro" $ require $ property $ do
      x <- forAll logicalGen
      tripping x (encodeValue schema'Logical) (decodeValueWithSchema (fromSchema schema'Logical))

