module Avro.EncodeAvroSpec
( spec
)
where

import           Data.Avro                     (decode)
import           Data.Avro.Encoding.DecodeAvro (decodeValueWithSchema)
import qualified Data.Avro.Encoding.EncodeAvro as Encode
import           Data.Avro.Schema              (resultToEither)
import           Data.Avro.Schema.ReadSchema   (fromSchema)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy

import Avro.Data.Endpoint
import Avro.Data.Unions

import Avro.TestUtils              (roundtripGen)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"    :: String) #-}
{-# ANN module ("HLint: ignore Redundant flip"  :: String) #-}

spec :: Spec
spec = describe "Avro.EncodeAvroSpec" $ do
  describe "Should encode directly and decode via old value" $ do
    it "Unions" $ require $ property $ do
      x <- forAll unionsGen
      tripping x (Encode.encodeAvro schema'Unions) (resultToEither . decode)

    it "Endpoint" $ require $ property $ do
      x <- forAll endpointGen
      tripping x (Encode.encodeAvro schema'Endpoint) (resultToEither . decode)

  describe "Should encode directly and decode via new value" $ do
    it "Unions" $ require $ property $ roundtripGen schema'Unions unionsGen
    it "Endpoint" $ require $ property $ roundtripGen schema'Endpoint endpointGen

