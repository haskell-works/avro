{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Avro.Encoding.ContainerSpec
where

import           Control.Lens
import           Control.Monad                 (join)
import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (eitherDecode, encode)
import           Data.Avro.Codec               (nullCodec)
import qualified Data.Avro.Encoding.Container  as Encoding
import           Data.Avro.Encoding.EncodeAvro (EncodeAvro)
import           Data.Avro.Encoding.Value      (DecodeAvro)
import           Data.Avro.Schema              (Schema)

import           Avro.TestUtils              (roundtripContainerGen)
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Hedgehog.Range              (Range)
import qualified Hedgehog.Range              as Range
import           Test.Hspec

import qualified Avro.Data.Endpoint as Endpoint
import qualified Avro.Data.Unions   as Unions

{-# ANN module ("HLint: ignore Redundant do"    :: String) #-}
{-# ANN module ("HLint: ignore Redundant flip"  :: String) #-}


spec :: Spec
spec = describe "Avro.Encoding.ContainerSpec" $ do
  describe "Roundtripping" $ do
    it "should roundtrip schema" $ require $ withTests 1 $ property $ do
      tripping Endpoint.schema'Endpoint encode eitherDecode
      tripping Unions.schema'Unions encode eitherDecode

    it "should roundtrip Endpoint" $ require $ property $ roundtripContainerGen Endpoint.schema'Endpoint Endpoint.endpointGen
    it "should roundtrip Unions"   $ require $ property $ roundtripContainerGen Unions.schema'Unions Unions.unionsGen

