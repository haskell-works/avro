{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.JSONSpec where

import Control.Monad (forM_)

import           Control.Monad.Identity (Identity (..))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Map               as Map

import Data.Avro.Deriving
import Data.Avro.EitherN
import Data.Avro.JSON

import Avro.Data.Endpoint
import Avro.Data.Enums
import Avro.Data.Reused
import Avro.Data.Unions

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Gen                as Gen
import Hedgehog.Range              as Range
import Paths_avro
import Test.Hspec

import System.Directory   (doesFileExist, getCurrentDirectory)
import System.Environment (setEnv)

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/unions-no-namespace.avsc"

spec :: Spec
spec = describe "Avro.JSONSpec: JSON serialization/parsing" $ do
  it "should do roundtrip (enums)" $ require $ property $ do
    msg <- forAll enumWrapperGen
    tripping msg  (Aeson.encode . toJSON) parseJSON

  it "should do roundtrip (reused)" $ require $ property $ do
    msg <- forAll reusedWrapperGen
    tripping msg  (Aeson.encode . toJSON) parseJSON

  it "should do roundtrip (small)" $ require $ property $ do
    msg <- forAll endpointGen
    tripping msg  (Aeson.encode . toJSON) parseJSON

  enumsExampleJSON <- runIO $ getFileName "test/data/enums-object.json" >>= LBS.readFile
  it "should parse (enums)" $ do
    let expected = EnumWrapper 37 "blarg" EnumReasonInstead
    parseJSON enumsExampleJSON `shouldBe` pure expected
  let unionsExampleA = Unions
        { unionsScalars     = Left "blarg"
        , unionsNullable    = Nothing
        , unionsRecords     = Left $ Foo { fooStuff = "stuff" }
        , unionsSameFields  = Left $ Foo { fooStuff = "foo stuff" }
        , unionsArrayAndMap = Left ["foo"]
        , unionsOne         = Identity 42
        , unionsThree       = E3_1 37
        , unionsFour        = E4_2 "foo"
        , unionsFive        = E5_4 $ Foo { fooStuff = "foo stuff" }
        , unionsSix         = E6_2 "foo"
        , unionsSeven       = E7_6 6.28
        , unionsEight       = E8_3 37
        , unionsNine        = E9_1 37
        , unionsTen         = E10_9 $ BS.pack [70, 79, 79, 66, 65, 82]
        }
      unionsExampleB = Unions
        { unionsScalars     = Right 37
        , unionsNullable    = Just 42
        , unionsRecords     = Right $ Bar { barStuff = "stuff"
                                          , barThings = Foo "things"
                                          }
        , unionsSameFields  = Right $ NotFoo { notFooStuff = "not foo stuff" }
        , unionsArrayAndMap = Right $ Map.fromList [("a", 5)]
        , unionsOne         = Identity 42
        , unionsThree       = E3_3 37
        , unionsFour        = E4_4 $ Foo { fooStuff = "foo stuff" }
        , unionsFive        = E5_5 $ NotFoo { notFooStuff = "not foo stuff" }
        , unionsSix         = E6_6 6.28
        , unionsSeven       = E7_7 False
        , unionsEight       = E8_8 2.718
        , unionsNine        = E9_9 $ BS.pack [70, 79, 79, 66, 65, 82]
        , unionsTen         = E10_10 $ Bar { barStuff = "bar stuff",
                                             barThings = Foo { fooStuff = "things" }
                                           }
        }
  it "should roundtrip (unions)" $ do
    forM_ [unionsExampleA, unionsExampleB] $ \ msg ->
      parseJSON (Aeson.encode (toJSON msg)) `shouldBe` pure msg
  unionsJsonA <- runIO $ getFileName "test/data/unions-object-a.json" >>= LBS.readFile
  unionsJsonB <- runIO $ getFileName "test/data/unions-object-b.json" >>= LBS.readFile
  it "should parse (unions)" $ do
    parseJSON unionsJsonA `shouldBe` pure unionsExampleA
    parseJSON unionsJsonB `shouldBe` pure unionsExampleB

  let unionsNoNamespaceA = UnionsNoNamespace (Left TypeA)
      unionsNoNamespaceB = UnionsNoNamespace (Right TypeB)
  it "should roundtrip (unions-no-namespace)" $ do
    parseJSON (Aeson.encode (toJSON unionsNoNamespaceA)) `shouldBe`
      pure unionsNoNamespaceA
    parseJSON (Aeson.encode (toJSON unionsNoNamespaceB)) `shouldBe`
      pure unionsNoNamespaceB
  let noNamespace = "test/data/unions-no-namespace-object.json"
      objectA = "{ \"unionField\" : { \"TypeA\" : {} } }"
      objectB = "{ \"unionField\" : { \"TypeB\" : {} } }"
  it "should parse (unions-no-namespace)" $ do
    parseJSON objectA `shouldBe` pure unionsNoNamespaceA
    parseJSON objectB `shouldBe` pure unionsNoNamespaceB

getFileName :: FilePath -> IO FilePath
getFileName p = do
  path <- getDataFileName p
  isOk <- doesFileExist path
  pure $ if isOk then path else p
