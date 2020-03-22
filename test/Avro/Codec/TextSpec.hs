{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Avro.Codec.TextSpec (spec) where

import           Avro.TestUtils
import           Data.ByteString.Lazy        (fromStrict)
import           Data.Text                   (Text)
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Hspec

import           Data.Avro                   (decodeValueWithSchema, encodeValueWithSchema)
import           Data.Avro.Deriving          (deriveAvroFromByteString, r)
import           Data.Avro.Schema.ReadSchema (fromSchema)
import qualified Data.Avro.Schema.Schema     as Schema

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "OnlyText",
  "namespace": "test.contract",
  "fields": [ {"name": "onlyTextValue", "type": "string"} ]
}
|]

spec :: Spec
spec = describe "Avro.Codec.TextSpec" $ do
  let schema = schema'OnlyText
  let readSchema = fromSchema schema
  it "Can decode \"This is an unit test\"" $ require $ withTests 1 $ property $ do
    -- The '(' here is the length (ASCII value) of the string
    let expectedBuffer = "(This is an unit test"
    let value = OnlyText "This is an unit test"
    encodeValueWithSchema schema value === expectedBuffer

  it "Can decode encoded Text values" $ require $ property $ do
    roundtripGen schema (OnlyText <$> Gen.text (Range.linear 0 128) Gen.alphaNum)

  it "Can process corrupted Text values without crashing" $ require $ property $ do
    bytes <- forAll $ Gen.bytes (Range.linear 0 511)
    eval $ decodeValueWithSchema @OnlyText readSchema (fromStrict bytes)
    success
