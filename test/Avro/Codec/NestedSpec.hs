{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Avro.Codec.NestedSpec (spec) where

import           Avro.TestUtils
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Hspec

import           Data.Avro.Deriving      (deriveAvroFromByteString, r)
import qualified Data.Avro.Schema.Schema as Schema

{- HLINT ignore "Redundant do"        -}

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "ParentType",
  "namespace": "test.contract",
  "fields": [
    {"name": "parentValue1", "type": "int" },
    {"name": "parentValue2", "type":
      {"type": "array",
       "items": {"type": "record", "name": "ChildType", "fields": [
         {"name": "childValue1", "type": "int"},
         {"name": "childValue2", "type": "int"}
       ]}
      }
    }
  ]
}
|]

childTypeGen :: MonadGen m => m ChildType
childTypeGen = ChildType <$> Gen.int32 Range.linearBounded <*> Gen.int32 Range.linearBounded

parentTypeGen :: MonadGen m => m ParentType
parentTypeGen = ParentType
  <$> Gen.int32 Range.linearBounded
  <*> Gen.list (Range.linear 0 100) childTypeGen

spec :: Spec
spec = describe "Avro.Codec.NestedSpec" $ do
  it "Can encode/decode nested structures" $ require $ property $ do
    roundtripGen schema'ParentType parentTypeGen

