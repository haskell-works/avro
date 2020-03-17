module Avro.Encoding.LogicalTypesSpec
where

import           Avro.Data.Logical
import           Data.Avro                     (decode, encode)
import           Data.Avro.Encoding.DecodeAvro (decodeValueWithSchema)
import qualified Data.Avro.Encoding.EncodeAvro as Encode
import           Data.Avro.Schema              (resultToEither)
import           Data.Avro.Schema.ReadSchema   (fromSchema)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"    :: String) #-}
{-# ANN module ("HLint: ignore Redundant flip"  :: String) #-}

spec :: Spec
spec = describe "Avro.Encoding.LogicalTypesSpec" $ do
  describe "Round-tripping" $ do
    xit "should encode with EncodeAvro and decode with FromAvro" $ require $ property $ do
      x <- forAll logicalGen
      tripping x (Encode.encodeAvro schema'Logical) (resultToEither . decode)

    xit "should encode with ToAvro and decode with DecodeAvro" $ require $ property $ do
      x <- forAll logicalGen
      tripping x encode (decodeValueWithSchema (fromSchema schema'Logical))

    it "shoule encode with EncodeAvro and decode with DecodeAvro" $ require $ property $ do
      x <- forAll logicalGen
      tripping x (Encode.encodeAvro schema'Logical) (decodeValueWithSchema (fromSchema schema'Logical))

