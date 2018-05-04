{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THUnionSpec
where

import qualified Data.List.NonEmpty as NE

import           Data.Avro
import           Data.Avro.Deriving
import qualified Data.Avro.Schema   as Schema
import qualified Data.Avro.Types    as Avro

import           Test.Hspec

deriveAvro "test/data/unions.avsc"

spec :: Spec
spec = describe "Avro.THUnionSpec: Schema with unions." $ do
  let objA = Unions
        { unionsScalars = Left "foo"
        , unionsNullable = Nothing
        , unionsRecords = Left $ Foo { fooStuff = "stuff" }
        , unionsSameFields = Left $ Foo { fooStuff = "more stuff" }
        }
      objB = Unions
        { unionsScalars = Right 42
        , unionsNullable = Just 37
        , unionsRecords = Right $ Bar { barStuff = "stuff"
                                      , barThings = Foo { fooStuff = "things" }
                                      }
        , unionsSameFields = Right $ NotFoo { notFooStuff = "different from Foo" }
        }

      field name schema def = Schema.Field name [] Nothing (Just Schema.Ascending) schema def
      record name namespace fields =
        Schema.Record name namespace [] Nothing (Just Schema.Ascending) fields
      named = Schema.NamedType . Schema.TN

      expectedSchema = record "Unions" (Just "haskell.avro.example")
        [ field "scalars"    (Schema.mkUnion (NE.fromList [Schema.String, Schema.Long])) scalarsDefault
        , field "nullable"   (Schema.mkUnion (NE.fromList [Schema.Null, Schema.Int]))    nullableDefault
        , field "records"    (Schema.mkUnion (NE.fromList [fooSchema, barSchema]))       Nothing
        , field "sameFields" (Schema.mkUnion (NE.fromList [named "Foo", notFooSchema]))  Nothing
        ]
      scalarsDefault  = Just $ Avro.Union (NE.fromList [Schema.String, Schema.Long]) Schema.String (Avro.String "foo")
      nullableDefault = Just $ Avro.Union (NE.fromList [Schema.Null, Schema.Int])    Schema.Null   Avro.Null

      fooSchema = record "Foo" Nothing [field "stuff" Schema.String Nothing]
      barSchema = record "Bar" Nothing
        [ field "stuff"  Schema.String Nothing
        , field "things" (named "Foo") Nothing
        ]
      notFooSchema = record "NotFoo" Nothing [field "stuff" Schema.String Nothing]
  it "produces valid schemas" $ do
    schema'Unions `shouldBe` expectedSchema
  it "records with unions should roundtrip" $ do
    fromAvro (toAvro objA) `shouldBe` pure objA
    fromAvro (toAvro objB) `shouldBe` pure objB
