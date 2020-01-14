module Avro.Encoding.LogicalTypesSpec
where

import           Avro.Data.Logical
import           Data.Avro                       (decode, encode)
import           Data.Avro.Encoding.FromEncoding (decodeValueWithSchema)
import qualified Data.Avro.Encoding.ToEncoding   as Encode
import           Data.Avro.Schema                (resultToEither)
import           Data.Avro.Schema.ReadSchema     (fromSchema)
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
    xit "should encode with ToEncoding and decode with FromAvro" $ require $ property $ do
      x <- forAll logicalGen
      tripping x (toLazyByteString . Encode.toEncoding schema'Logical) (resultToEither . decode)

    xit "should encode with ToAvro and decode with FromEncoding" $ require $ property $ do
      x <- forAll logicalGen
      tripping x encode (decodeValueWithSchema (fromSchema schema'Logical))

    it "shoule encode with ToEncoding and decode with FromEncoding" $ require $ property $ do
      x <- forAll logicalGen
      tripping x (toLazyByteString . Encode.toEncoding schema'Logical) (decodeValueWithSchema (fromSchema schema'Logical))

