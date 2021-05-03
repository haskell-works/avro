{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Avro.Encoding.ContainerSpec
where

import Data.Aeson              (eitherDecode, encode)
import Data.Avro.Codec         (nullCodec)
import Data.Avro.Schema.Schema (Schema)

import           Avro.TestUtils              (roundtripContainerGen)
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Hedgehog.Range              (Range)
import qualified Hedgehog.Range              as Range
import           Test.Hspec

import qualified Avro.Data.Endpoint as Endpoint
import qualified Avro.Data.Unions   as Unions

{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Avro.Encoding.ContainerSpec" $ do
  describe "Roundtripping" $ do
    it "should roundtrip schema" $ require $ withTests 1 $ property $ do
      tripping Endpoint.schema'Endpoint encode eitherDecode
      tripping Unions.schema'Unions encode eitherDecode

    it "should roundtrip Endpoint" $ require $ property $ roundtripContainerGen Endpoint.schema'Endpoint Endpoint.endpointGen
    it "should roundtrip Unions"   $ require $ property $ roundtripContainerGen Unions.schema'Unions Unions.unionsGen

