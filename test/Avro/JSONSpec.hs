{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.JSONSpec where

import           Control.Monad        (forM_)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS

import           Data.Avro.Deriving
import           Data.Avro.JSON

import           Test.Hspec

import           Paths_avro

deriveAvro "test/data/enums.avsc"
deriveAvro "test/data/reused.avsc"
deriveAvro "test/data/small.avsc"
deriveAvro "test/data/unions.avsc"

spec :: Spec
spec = describe "Avro.JSONSpec: JSON serialization/parsing" $ do
  it "should do roundtrip (enums)" $ do
    let msg = EnumWrapper
          { enumWrapperId = 42
          , enumWrapperName = "Text"
          , enumWrapperReason = EnumReasonBecause
          }
    parseJSON (Aeson.encode (toJSON msg)) `shouldBe` pure msg
  it "should do roundtrip (reused)" $ do
    let msg = ReusedWrapper
          { reusedWrapperFull  = ReusedChild 42
          , reusedWrapperInner = ContainerChild
            { containerChildFstIncluded = ReusedChild 37
            , containerChildSndIncluded = ReusedChild 64
            }
          }
    parseJSON (Aeson.encode (toJSON msg)) `shouldBe` pure msg
  it "should do roundtrip (small)" $ do
    let msgs =
          [ Endpoint
            { endpointIps         = ["192.168.1.1", "127.0.0.1"]
            , endpointPorts       = [PortRange 1 10, PortRange 11 20]
            , endpointOpaque      = Opaque "16-b-long-string"
            , endpointCorrelation = Opaque "opaq-correlation"
            , endpointTag         = Left 14
            }
          , Endpoint
            { endpointIps         = []
            , endpointPorts       = [PortRange 1 10, PortRange 11 20]
            , endpointOpaque      = Opaque "opaque-long-text"
            , endpointCorrelation = Opaque "correlation-data"
            , endpointTag         = Right "first-tag"
            }
          ]
    forM_ msgs $ \ msg ->
      parseJSON (Aeson.encode (toJSON msg)) `shouldBe` pure msg
  enumsExampleJSON <- runIO $ getDataFileName "test/data/enums-object.json" >>= LBS.readFile
  it "should parse (enums)" $ do
    let expected = EnumWrapper 37 "blarg" EnumReasonInstead
    parseJSON enumsExampleJSON `shouldBe` pure expected
  let unionsExampleA = Unions
        { unionsScalars = Left "blarg"
        , unionsNullable = Nothing
        , unionsRecords = Left $ Foo { fooStuff = "stuff" }
        , unionsSameFields = Left $ Foo { fooStuff = "foo stuff" }
        }
      unionsExampleB = Unions
        { unionsScalars = Right 37
        , unionsNullable = Just 42
        , unionsRecords = Right $ Bar { barStuff = "stuff"
                                      , barThings = Foo "things"
                                      }
        , unionsSameFields = Right $ NotFoo { notFooStuff = "not foo stuff" }
        }
  it "should roundtrip (unions)" $ do
    forM_ [unionsExampleA, unionsExampleB] $ \ msg ->
      parseJSON (Aeson.encode (toJSON msg)) `shouldBe` pure msg
  unionsJsonA <- runIO $ getDataFileName "test/data/unions-object-a.json" >>= LBS.readFile
  unionsJsonB <- runIO $ getDataFileName "test/data/unions-object-b.json" >>= LBS.readFile
  it "should parse (unions)" $ do
    parseJSON unionsJsonA `shouldBe` pure unionsExampleA
    parseJSON unionsJsonB `shouldBe` pure unionsExampleB
