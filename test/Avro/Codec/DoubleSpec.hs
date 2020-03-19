{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}
module Avro.Codec.DoubleSpec (spec) where

import           Avro.TestUtils
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Hspec

import           Data.Avro               (encodeValue)
import           Data.Avro.Deriving      (deriveAvroFromByteString, r)
import qualified Data.Avro.Schema.Schema as Schema
import qualified Data.ByteString.Lazy    as BL

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "OnlyDouble",
  "namespace": "test.contract",
  "fields": [ {"name": "onlyDoubleValue", "type": "double"} ]
}
|]

spec :: Spec
spec = describe "Avro.Codec.DoubleSpec" $ do
  it "Can decode 0.89" $ require $ withTests 1 $ property $ do
    let expectedBuffer = BL.pack [123, 20, -82, 71, -31, 122, -20, 63]
    encodeValue Schema.Double (OnlyDouble 0.89) === expectedBuffer

  it "Can decode -2.0" $ require $ withTests 1 $ property $ do
    let expectedBuffer = BL.pack [0, 0, 0, 0, 0, 0, 0, -64]
    encodeValue Schema.Double (OnlyDouble (-2.0)) === expectedBuffer

  it "Can decode 1.0" $ require $ withTests 1 $ property $ do
    let expectedBuffer = BL.pack [0, 0, 0, 0, 0, 0, -16, 63]
    encodeValue Schema.Double (OnlyDouble 1.0) === expectedBuffer

  it "Can decode encoded Double values" $ require $ property $ do
    roundtripGen Schema.Double (Gen.double (Range.linearFrac (-27000.0) 27000.0))
