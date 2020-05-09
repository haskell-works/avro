{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Avro.Codec.NullSpec (spec) where

import           Avro.TestUtils
import qualified Data.Avro.Schema.Schema     as Schema
import           Data.Text                   (Text)
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Test.Hspec

import qualified Data.ByteString.Lazy as BL

import Data.Avro          (encodeValueWithSchema)
import Data.Avro.Deriving (deriveAvroFromByteString, r)
import Data.Avro.EitherN  (Either3 (E3_1, E3_3))

{- HLINT ignore "Redundant do"        -}

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "OnlyNull",
  "namespace": "test.contract",
  "fields": [ {"name": "onlyNullValue", "type": "null"} ]
}
|]

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "NullInUnion1",
  "namespace": "test.contract",
  "fields": [ {"name": "nullInUnion1", "type": ["null", "string", "boolean"] } ]
}
|]

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "NullInUnion3",
  "namespace": "test.contract",
  "fields": [ {"name": "nullInUnion3", "type": ["string", "boolean", "null"] } ]
}
|]

nullEncoding :: BL.ByteString
nullEncoding = mempty

spec :: Spec
spec = describe "Avro.Codec.NullSpec" $ do
  it "should encode () correctly" $ require $ withTests 1 $ property $ do
    encodeValueWithSchema Schema.Null (OnlyNull ()) === nullEncoding

  it "should encode then decode () correctly" $ require $ withTests 1 $ property $ do
    roundtripGen Schema.Null (pure ())

  it "should encode () in union (first) correctly" $ require $ withTests 1 $ property $ do
    let index = encodeValueWithSchema Schema.Int' (0 :: Int)
        nullFirstEncoding = index <> nullEncoding
    encodeValueWithSchema
      (Schema.Union [Schema.Null, Schema.String', Schema.Boolean])
      (E3_1 () :: Either3 () Text Bool)
      === nullFirstEncoding

  it "should encode then decode () in union (first) correctly" $ require $ withTests 1 $ property $ do
    roundtripGen
      (Schema.Union [Schema.Null, Schema.String', Schema.Boolean])
      (pure (E3_1 () :: Either3 () Text Bool))

  it "should encode () in union (last) correctly" $ require $ withTests 1 $ property $ do
    let index = encodeValueWithSchema Schema.Int' (2 :: Int)
        nullLastEncoding = index <> nullEncoding
    encodeValueWithSchema
      (Schema.Union [Schema.String', Schema.Boolean, Schema.Null])
      (E3_3 () :: Either3 Text Bool ())
      === nullLastEncoding

  it "should encode then decode () in union (last) correctly" $ require $ withTests 1 $ property $ do
    roundtripGen
      (Schema.Union [Schema.String', Schema.Boolean, Schema.Null])
      (pure (E3_3 () :: Either3 Text Bool ()))
