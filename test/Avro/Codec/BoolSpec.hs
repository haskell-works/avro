{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Avro.Codec.BoolSpec (spec) where

import           Avro.TestUtils
import qualified Data.Avro.Schema.Schema     as Schema
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Test.Hspec

import qualified Data.ByteString.Lazy as BL

import Data.Avro          (encodeValueWithSchema)
import Data.Avro.Deriving (deriveAvroFromByteString, r)

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "OnlyBool",
  "namespace": "test.contract",
  "fields": [ {"name": "onlyBoolValue", "type": "boolean"} ]
}
|]

spec :: Spec
spec = describe "Avro.Codec.BoolSpec" $ do
  it "should encode True correctly" $ require $ withTests 1 $ property $ do
    let trueEncoding = BL.singleton 0x01
    encodeValueWithSchema Schema.Boolean (OnlyBool True) === trueEncoding

  it "should encode False correctly" $ require $ withTests 1 $ property $ do
    let falseEncoding = BL.singleton 0x00
    encodeValueWithSchema Schema.Boolean (OnlyBool False) === falseEncoding

  it "should encode then decode True correctly" $ require $ withTests 10 $ property $ do
    roundtripGen Schema.Boolean Gen.bool
