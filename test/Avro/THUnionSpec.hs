{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THUnionSpec
where

import           Data.Avro
import           Data.Avro.Deriving

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
  it "records with unions should roundtrip" $ do
    fromAvro (toAvro objA) `shouldBe` pure objA
    fromAvro (toAvro objB) `shouldBe` pure objB
