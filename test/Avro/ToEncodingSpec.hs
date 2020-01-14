module Avro.ToEncodingSpec
( spec
)
where

import           Data.Avro                       (decode)
import           Data.Avro.Encoding.FromEncoding (decodeValueWithSchema)
import qualified Data.Avro.Encoding.ToEncoding   as Encode
import           Data.Avro.Schema                (resultToEither)
import           Data.Avro.Schema.ReadSchema     (fromSchema)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy

import Avro.Data.Endpoint
import Avro.Data.Unions

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"    :: String) #-}
{-# ANN module ("HLint: ignore Redundant flip"  :: String) #-}

spec :: Spec
spec = describe "Avro.ToEncodingSpec" $ do
  describe "Should encode directly and decode via old value" $ do
    it "Unions" $ require $ property $ do
      x <- forAll unionsGen
      tripping x (toLazyByteString . Encode.toEncoding schema'Unions) (resultToEither . decode)

    it "Endpoint" $ require $ property $ do
      x <- forAll endpointGen
      tripping x (toLazyByteString . Encode.toEncoding schema'Endpoint) (resultToEither . decode)

  describe "Should encode directly and decode via new value" $ do
    it "Unions" $ require $ property $ do
      x <- forAll unionsGen
      tripping x (toLazyByteString . Encode.toEncoding schema'Unions) (decodeValueWithSchema (fromSchema schema'Unions))

    it "Endpoint" $ require $ property $ do
      x <- forAll endpointGen
      tripping x (toLazyByteString . Encode.toEncoding schema'Endpoint) (decodeValueWithSchema (fromSchema schema'Endpoint))
