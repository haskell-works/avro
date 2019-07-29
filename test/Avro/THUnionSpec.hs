{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THUnionSpec
where

import qualified Data.List.NonEmpty as NE

import qualified Data.Aeson           as Aeson
import           Data.Avro
import           Data.Avro.Deriving
import           Data.Avro.EitherN
import qualified Data.Avro.Schema     as Schema
import qualified Data.Avro.Types      as Avro
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map             as Map
import qualified Data.Vector          as V

import System.Directory (doesFileExist)

import Test.Hspec

import Paths_avro

deriveAvro "test/data/unions.avsc"

spec :: Spec
spec = describe "Avro.THUnionSpec: Schema with unions." $ do
  let objA = Unions
        { unionsScalars     = Left "foo"
        , unionsNullable    = Nothing
        , unionsRecords     = Left $ Foo { fooStuff = "stuff" }
        , unionsSameFields  = Left $ Foo { fooStuff = "more stuff" }
        , unionsArrayAndMap = Left ["foo"]
        , unionsThree       = E3_1 37
        , unionsFour        = E4_2 "foo"
        , unionsFive        = E5_4 $ Foo { fooStuff = "foo stuff" }
        }
      objB = Unions
        { unionsScalars     = Right 42
        , unionsNullable    = Just 37
        , unionsRecords     = Right $ Bar { barStuff  = "stuff"
                                          , barThings = Foo { fooStuff = "things" }
                                          }
        , unionsSameFields  = Right $ NotFoo { notFooStuff = "different from Foo" }
        , unionsArrayAndMap = Right $ Map.fromList [("a", 5)]
        , unionsThree       = E3_3 37
        , unionsFour        = E4_4 $ Foo { fooStuff = "foo stuff" }
        , unionsFive        = E5_5 $ NotFoo { notFooStuff = "not foo stuff" }
        }

      field name schema def = Schema.Field name [] Nothing (Just Schema.Ascending) schema def
      record name fields =
        Schema.Record name [] Nothing (Just Schema.Ascending) fields
      named = Schema.NamedType

      foo    = named "haskell.avro.example.Foo"
      notFoo = named "haskell.avro.example.NotFoo"
      expectedSchema = record "haskell.avro.example.Unions"
        [ field "scalars"     (Schema.mkUnion (NE.fromList [Schema.String, Schema.Long])) scalarsDefault
        , field "nullable"    (Schema.mkUnion (NE.fromList [Schema.Null, Schema.Int]))    nullableDefault
        , field "records"     (Schema.mkUnion (NE.fromList [fooSchema, barSchema]))       Nothing
        , field "sameFields"  (Schema.mkUnion (NE.fromList [foo, notFooSchema]))          Nothing
        , field "arrayAndMap" (Schema.mkUnion (NE.fromList [array, map]))                 Nothing

        , field "three" (Schema.mkUnion (NE.fromList [Schema.Int, Schema.String, Schema.Long]))              Nothing
        , field "four"  (Schema.mkUnion (NE.fromList [Schema.Int, Schema.String, Schema.Long, foo]))         Nothing
        , field "five"  (Schema.mkUnion (NE.fromList [Schema.Int, Schema.String, Schema.Long, foo, notFoo])) Nothing
        ]
      scalarsDefault  = Just $ Avro.Union (V.fromList [Schema.String, Schema.Long]) Schema.String (Avro.String "foo")
      nullableDefault = Just $ Avro.Union (V.fromList [Schema.Null, Schema.Int])    Schema.Null   Avro.Null

      fooSchema = record "haskell.avro.example.Foo" [field "stuff" Schema.String Nothing]
      barSchema = record "haskell.avro.example.Bar"
        [ field "stuff"  Schema.String Nothing
        , field "things" (named "haskell.avro.example.Foo") Nothing
        ]
      notFooSchema = record "haskell.avro.example.NotFoo" [field "stuff" Schema.String Nothing]

      array = Schema.Array { Schema.item = Schema.String }
      map   = Schema.Map { Schema.values = Schema.Long }

  unionsSchemaFile <- runIO $ getFileName "test/data/unions.avsc" >>= LBS.readFile
  let Just unionsSchemaFromJSON = Aeson.decode unionsSchemaFile

  it "produces valid schemas" $ do
    schema'Unions        `shouldBe` expectedSchema
    unionsSchemaFromJSON `shouldBe` expectedSchema
  it "records with unions should roundtrip" $ do
    fromAvro (toAvro objA) `shouldBe` pure objA
    fromAvro (toAvro objB) `shouldBe` pure objB

getFileName :: FilePath -> IO FilePath
getFileName p = do
  path <- getDataFileName p
  isOk <- doesFileExist path
  pure $ if isOk then path else p
