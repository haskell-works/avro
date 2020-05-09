{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Avro.Codec.MaybeSpec (spec) where

import           Avro.TestUtils
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Test.Hspec

import           Data.Avro.Deriving      (deriveAvroFromByteString, r)
import qualified Data.Avro.Schema.Schema as Schema

{- HLINT ignore "Redundant do"        -}

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "OnlyMaybeBool",
  "namespace": "test.contract",
  "fields": [ {"name": "onlyMaybeBoolValue", "type": ["null", "boolean"]} ]
}
|]

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "OnlyMaybeBool'",
  "namespace": "test.contract",
  "fields": [ {"name": "onlyMaybeBoolValue", "type": ["boolean", "null"]} ]
}
|]

spec :: Spec
spec = describe "Avro.Codec.MaybeSpec" $ do
  it "should encode then decode Maybe Bool correctly" $ require $ property $ do
    roundtripGen schema'OnlyMaybeBool (OnlyMaybeBool <$> Gen.maybe Gen.bool)

  it "should be able to roundtrip a Maybe Bool defined with flipped union type" $ require $ property $ do
    roundtripGen schema'OnlyMaybeBool' (OnlyMaybeBool' <$> Gen.maybe Gen.bool)
