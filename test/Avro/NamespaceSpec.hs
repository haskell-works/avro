{-# LANGUAGE OverloadedStrings #-}
module Avro.NamespaceSpec where

import           Control.Monad        (forM_)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS

import           System.Directory     (doesFileExist, getCurrentDirectory)
import           System.Environment   (setEnv)

import           Test.Hspec

import           Paths_avro

import           Data.Avro.Schema

spec :: Spec
spec = describe "NamespaceSpec.hs: namespace inference in Avro schemas" $ do
  schemas <- runIO $ getFileName "test/data/namespace-inference.json" >>= LBS.readFile
  let parsedSchemas :: [Schema]
      Just parsedSchemas = Aeson.decode schemas
  it "should infer namespaces correctly" $ do
    forM_ parsedSchemas (`shouldBe` expected)
  it "should generate JSON with namespaces inferred" $ do
    -- the first schema in namespace-ifnerence.json is in the exact
    -- format we expect to serialize Schema values
    let expectedJSONSchema :: Aeson.Value
        Just expectedJSONSchema = head <$> Aeson.decode schemas
    Aeson.toJSON expected `shouldBe` expectedJSONSchema

expected :: Schema
expected = Record
  { name    = "com.example.Foo"
  , aliases = ["com.example.FooBar", "com.example.not.Bar"]
  , doc     = Just "An example schema to test namespace handling."
  , order   = Just Ascending
  , fields  = [field "bar" bar, field "baz" $ NamedType "com.example.baz.Baz"]
  }
  where field name schema = Field name [] Nothing (Just Ascending) schema Nothing

        bar = Record
          { name    = "com.example.Bar"
          , aliases = ["com.example.Bar2", "com.example.not.Foo"]
          , doc     = Nothing
          , order   = Just Ascending
          , fields  = [ field "baz" baz
                      , field "bazzy" $ NamedType "com.example.Bazzy"
                      ]
          }

        baz = Record
          { name    = "com.example.baz.Baz"
          , aliases = ["com.example.Bazzy"]
          , doc     = Nothing
          , order   = Just Ascending
          , fields  = [ field "baz"   $ NamedType "com.example.baz.Baz"
                      , field "bazzy" $ NamedType "com.example.Bazzy"
                      ]
          }

getFileName :: FilePath -> IO FilePath
getFileName p = do
  path <- getDataFileName p
  isOk <- doesFileExist path
  pure $ if isOk then path else p
