{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Avro.Codec.FloatSpec (spec) where

import           Avro.TestUtils
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Hspec

import           Data.Avro               (encodeValueWithSchema)
import           Data.Avro.Deriving      (deriveAvroFromByteString, r)
import qualified Data.Avro.Schema.Schema as Schema
import qualified Data.ByteString.Lazy    as BL

{- HLINT ignore "Redundant do"        -}

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "OnlyFloat",
  "namespace": "test.contract",
  "fields": [ {"name": "onlyFloatValue", "type": "float"} ]
}
|]

spec :: Spec
spec = describe "Avro.Codec.FloatSpec" $ do
  it "Can decode 0.89" $ require $ withTests 1 $ property $ do
    let expectedBuffer = BL.pack [10, -41, 99, 63]
    let value = OnlyFloat 0.89
    encodeValueWithSchema schema'OnlyFloat value === expectedBuffer

  it "Can decode -2.0" $ require $ withTests 1 $ property $ do
    let expectedBuffer = BL.pack [0, 0, 0, -64]
    let value = OnlyFloat (-2.0)
    encodeValueWithSchema schema'OnlyFloat value === expectedBuffer

  it "Can decode 1.0" $ require $ withTests 1 $ property $ do
    let expectedBuffer = BL.pack [0, 0, 128, 63]
    let value = OnlyFloat 1.0
    encodeValueWithSchema schema'OnlyFloat value === expectedBuffer

  it "Can decode encoded Float values" $ require $ property $ do
    roundtripGen schema'OnlyFloat (OnlyFloat <$> Gen.float (Range.linearFrac (-27000.0) 27000.0))
