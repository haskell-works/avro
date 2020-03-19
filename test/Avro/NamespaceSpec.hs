{-# LANGUAGE OverloadedStrings #-}
module Avro.NamespaceSpec where

import Control.Monad (forM_)

import qualified Data.Aeson              as Aeson
import           Data.Avro.Schema.Schema
import qualified Data.ByteString.Lazy    as LBS

import System.Directory   (doesFileExist, getCurrentDirectory)
import System.Environment (setEnv)

import Paths_avro
import Test.Hspec


{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

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
  it "should render names in the null namespace with no leading '.'" $
    renderFullname (TN "FooType" []) `shouldBe` "FooType"
  nullNamespaceSchema <- runIO $ getFileName "test/data/null-namespace.json" >>= LBS.readFile
  it "should generate JSON with null namespaces rendered correctly" $
    Aeson.decode nullNamespaceSchema `shouldBe` Just expectedNullNamespace

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


expectedNullNamespace :: Schema
expectedNullNamespace = Record
  { name    = "Foo"
  , aliases = []
  , doc     = Just "An example schema to test null namespace handling."
  , order   = Just Ascending
  , fields  = [field "bar" $ NamedType "Bar", field "baz" $ NamedType "com.example.Baz"]
  }
  where field name schema = Field name [] Nothing (Just Ascending) schema Nothing


getFileName :: FilePath -> IO FilePath
getFileName p = do
  path <- getDataFileName p
  isOk <- doesFileExist path
  pure $ if isOk then path else p
