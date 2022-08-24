{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Avro.JSONSpec where

import Control.Monad (forM_)

import           Control.Monad.Identity (Identity (..))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Either            (isRight)
import           Data.Int
import qualified Data.Map               as Map
import qualified Data.HashMap.Strict    as HM
import           Data.Tagged
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import Data.Avro.Deriving
import Data.Avro.EitherN
import Data.Avro.Encoding.FromAvro
import Data.Avro.JSON
import Data.Avro.HasAvroSchema
import Data.Avro.Schema.Deconflict (deconflict)
import qualified Data.Avro.Schema.Schema as S
import Data.Avro.Schema.ReadSchema

import qualified Avro.Data.Deconflict.Read as Read
import qualified Avro.Data.Deconflict.Write as Write
import Avro.Data.Endpoint
import Avro.Data.Enums
import Avro.Data.FixedTypes
import Avro.Data.Karma
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

{- HLINT ignore "Redundant do"        -}

deriveAvro "test/data/unions-no-namespace.avsc"
deriveAvro "test/data/maybe.avsc"

roundtripGen :: (Monad m, Show a, Eq a, ToAvroJSON a, FromAvro a) => S.Schema -> Gen a -> PropertyT m ()
roundtripGen sch gen = do
  value <- forAll gen
  tripping value (toAvroJSON sch) (fromJSON (fromSchema sch)) 

roundtripGenKnownSchema :: (Monad m, Show a, Eq a, ToAvroJSON a, FromAvro a, HasAvroSchema a) => Gen a -> PropertyT m ()
roundtripGenKnownSchema gen = do
  value <- forAll gen
  let sch = schemaOf value
  tripping value (toAvroJSON sch) (fromJSON (fromSchema sch)) 



spec :: Spec
spec = describe "Avro.JSONSpec: JSON serialization/parsing" $ do
  it "should pass" $ require $ withTests 1 $ property success

  describe "ToAvroJSON instances" $ do
    specify "Int" $ do
      let int = 1 :: Int
      toAvroJSON (S.Long Nothing) int `shouldBe` Aeson.Number 1
      toAvroJSON (S.Int Nothing) int `shouldBe` Aeson.Number 1
    specify "Int32" $ do
      let int = 1 :: Int32
      toAvroJSON (S.Long Nothing) int `shouldBe` Aeson.Number 1
      toAvroJSON (S.Int Nothing) int `shouldBe` Aeson.Number 1

  describe "Schema evolution" $ do
    it "can read values in older formats from new schemas" $ do
      let writeSchema = unTagged $ schema @Write.Foo
          readSchema = unTagged $ schema @Read.Foo
          (Right deconflicted) = deconflict writeSchema readSchema
      require $ property $ do
        foo <- forAll Write.genFoo
        let jsonAnnotation = TL.unpack $ TL.decodeUtf8 $ Aeson.encode $ toJSON foo
        annotate jsonAnnotation
        annotate $ show deconflicted
        case  fromJSON @Read.Foo deconflicted (toJSON foo) of
          Left err -> annotate err >> failure
          Right _ -> success
  describe "Roundtrip" $ do
    it "Null roundtrip" $ require $ withTests 1 $ property $ do
      roundtripGen S.Null (Gen.constant ())
    it "Bool roundtrip" $ require $ withTests 2 $ property $ do
      roundtripGen S.Boolean Gen.bool_
    it "Int roundtrip" $ require $ property $ do
      roundtripGen (S.Int Nothing) (Gen.int32 Range.linearBounded)
    it "Long roundtrip" $ require $ property $ do
      roundtripGen (S.Long Nothing) (Gen.int Range.linearBounded)
      roundtripGen (S.Long Nothing) (Gen.int64 Range.linearBounded)
    it "Float roundtrip" $ require $ property $ do
      roundtripGen S.Float (Gen.float (Range.exponentialFloat (-500) 500))
    it "Double roundtrip" $ require $ property $ do
      roundtripGen S.Double (Gen.double (Range.exponentialFloat (-500) 500))
    it "Bytes roundtrip" $ require $ property $ do
      roundtripGen (S.Bytes Nothing) (Gen.bytes (Range.linear 0 100))
    it "String roundtrip" $ require $ property $ do
      roundtripGen (S.String Nothing) (Gen.text (Range.linear 0 100) Gen.unicodeAll)
    it "Array roundtrip" $ require $ property $ do
      roundtripGen (S.Array $ S.Long Nothing) (Gen.list (Range.linear 0 100) $ Gen.int Range.linearBounded)
    it "Map roundtrip" $ require $ property $ do
      let gen = do
            items <- Gen.list 
              (Range.linear 0 100) 
              (do
                k <- Gen.text (Range.linear 1 100) Gen.alphaNum 
                v <- Gen.int Range.linearBounded
                pure (k, v)
              )
            pure $ HM.fromList items
      roundtripGen (S.Map $ S.Long Nothing) gen
    -- TODO named types
    -- TODO record types
    describe "Record roundtrip" $ do
      specify "Endpoint" $ require $ property $ roundtripGenKnownSchema endpointGen
      specify "Blessing" $ require $ property $ roundtripGenKnownSchema blessingGen
      specify "Curse" $ require $ property $ roundtripGenKnownSchema curseGen
      specify "ReuseFixed" $ require $ property $ roundtripGenKnownSchema reuseFixedGen
      specify "Unions" $ require $ property $ roundtripGenKnownSchema unionsGen

    it "Enum roundtrip" $ require $ property $ do
      roundtripGen (schemaOf EnumReasonBecause) (Gen.enumBounded :: Gen EnumReason)
    -- TODO it "Union roundtrip"
    it "Fixed roundtrip" $ require $ property $ do
      roundtripGen (schemaOf $ FixedTag "wow") (fmap FixedTag $ Gen.bytes $ Range.singleton 3)

  -- it "should do roundtrip (enums)" $ require $ property $ do
  --   msg <- forAll enumWrapperGen
  --   tripping msg  (Aeson.encode . toJSON) parseJSON

  -- it "should do roundtrip (reused)" $ require $ property $ do
  --   msg <- forAll reusedWrapperGen
  --   tripping msg  (Aeson.encode . toJSON) parseJSON

  -- it "should do roundtrip (small)" $ require $ property $ do
  --   msg <- forAll endpointGen
  --   tripping msg  (Aeson.encode . toJSON) parseJSON

  -- enumsExampleJSON <- runIO $ getFileName "test/data/enums-object.json" >>= LBS.readFile
  -- it "should parse (enums)" $ do
  --   let expected = EnumWrapper 37 "blarg" EnumReasonInstead
  --   parseJSON enumsExampleJSON `shouldBe` pure expected
  -- let unionsExampleA = Unions
  --       { unionsScalars     = Left "blarg"
  --       , unionsNullable    = Nothing
  --       , unionsRecords     = Left $ Foo { fooStuff = "stuff" }
  --       , unionsSameFields  = Left $ Foo { fooStuff = "foo stuff" }
  --       , unionsArrayAndMap = Left ["foo"]
  --       , unionsOne         = Identity 42
  --       , unionsThree       = E3_1 37
  --       , unionsFour        = E4_2 "foo"
  --       , unionsFive        = E5_4 $ Foo { fooStuff = "foo stuff" }
  --       , unionsSix         = E6_2 "foo"
  --       , unionsSeven       = E7_6 6.28
  --       , unionsEight       = E8_3 37
  --       , unionsNine        = E9_1 37
  --       , unionsTen         = E10_9 $ BS.pack [70, 79, 79, 66, 65, 82]
  --       }
  --     unionsExampleB = Unions
  --       { unionsScalars     = Right 37
  --       , unionsNullable    = Just 42
  --       , unionsRecords     = Right $ Bar { barStuff = "stuff"
  --                                         , barThings = Foo "things"
  --                                         }
  --       , unionsSameFields  = Right $ NotFoo { notFooStuff = "not foo stuff" }
  --       , unionsArrayAndMap = Right $ Map.fromList [("a", 5)]
  --       , unionsOne         = Identity 42
  --       , unionsThree       = E3_3 37
  --       , unionsFour        = E4_4 $ Foo { fooStuff = "foo stuff" }
  --       , unionsFive        = E5_5 $ NotFoo { notFooStuff = "not foo stuff" }
  --       , unionsSix         = E6_6 6.28
  --       , unionsSeven       = E7_7 False
  --       , unionsEight       = E8_8 2.718
  --       , unionsNine        = E9_9 $ BS.pack [70, 79, 79, 66, 65, 82]
  --       , unionsTen         = E10_10 $ Bar { barStuff = "bar stuff",
  --                                            barThings = Foo { fooStuff = "things" }
  --                                          }
  --       }
  -- it "should roundtrip (unions)" $ do
  --   forM_ [unionsExampleA, unionsExampleB] $ \ msg ->
  --     parseJSON (Aeson.encode (toJSON msg)) `shouldBe` pure msg
  -- unionsJsonA <- runIO $ getFileName "test/data/unions-object-a.json" >>= LBS.readFile
  -- unionsJsonB <- runIO $ getFileName "test/data/unions-object-b.json" >>= LBS.readFile
  -- it "should parse (unions)" $ do
  --   parseJSON unionsJsonA `shouldBe` pure unionsExampleA
  --   parseJSON unionsJsonB `shouldBe` pure unionsExampleB

  -- let unionsNoNamespaceA = UnionsNoNamespace (Left TypeA)
  --     unionsNoNamespaceB = UnionsNoNamespace (Right TypeB)
  -- it "should roundtrip (unions-no-namespace)" $ do
  --   parseJSON (Aeson.encode (toJSON unionsNoNamespaceA)) `shouldBe`
  --     pure unionsNoNamespaceA
  --   parseJSON (Aeson.encode (toJSON unionsNoNamespaceB)) `shouldBe`
  --     pure unionsNoNamespaceB
  -- let noNamespace = "test/data/unions-no-namespace-object.json"
  --     objectA = "{ \"unionField\" : { \"TypeA\" : {} } }"
  --     objectB = "{ \"unionField\" : { \"TypeB\" : {} } }"
  -- it "should parse (unions-no-namespace)" $ do
  --   parseJSON objectA `shouldBe` pure unionsNoNamespaceA
  --   parseJSON objectB `shouldBe` pure unionsNoNamespaceB

getFileName :: FilePath -> IO FilePath
getFileName p = do
  path <- getDataFileName p
  isOk <- doesFileExist path
  pure $ if isOk then path else p
