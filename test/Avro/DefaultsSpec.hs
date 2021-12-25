{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Avro.DefaultsSpec
where

import qualified Data.Aeson              as J
import qualified Data.Aeson.KeyMap       as KM
import           Data.Avro.Schema.Schema
import qualified Data.HashMap.Strict     as M
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.Vector             as V

import Avro.Data.Maybe

import Avro.TestUtils              (roundtripGen)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Avro.DefaultsSpec: Schema with named types" $ do
  it "should decode value" $ require $ property $ roundtripGen schema'MaybeTest maybeTestGen

  it "should read default from Schema" $
    let
      msgSchema = schema'MaybeTest
      fixedSchema = schema'FixedTag
      defaults = fldDefault <$> fields msgSchema
    in defaults `shouldBe` [ Just $ DUnion (V.fromList [Null, String']) Null DNull
                           , Just $ DFixed fixedSchema "\0\42\255"
                           , Just $ DBytes Bytes' "\0\37\255"
                           ]

  it "should encode schema with default" $
    let
      msgSchema = schema'MaybeTest
      (J.Object jSchema) = J.toJSON msgSchema
      (Just (J.Array flds)) = KM.lookup "fields" jSchema
      (J.Object jFld) = V.head flds
    in KM.lookup "default" jFld `shouldBe` Just J.Null
