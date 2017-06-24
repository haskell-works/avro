{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Avro.SchemaSpec
where

import Data.Avro
import Data.Avro.Schema
import Data.Maybe
import Test.Hspec
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS


{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.SchemaSpec: ExtractRecords" $ do
  it "should extract records and resolve names" $ do
    schBS <- LBS.readFile "test/data/reused.avsc"
    let mbSchema = J.decode schBS :: Maybe Schema
    mbSchema `shouldSatisfy` isJust
    case mbSchema of
      Nothing -> fail "Unable to decode schema"
      Just sch -> do
        let [a, b, c] = extractRecords sch

        a `shouldBe` sch
        take 1 (fldType <$> fields a) `shouldBe` [b]

        let [fstInc, sndInc] = fldType <$> fields c
        fstInc `shouldSatisfy` isRecord
        sndInc `shouldSatisfy` isNamedType
        typeName fstInc `shouldBe` typeName sndInc



isNamedType :: Schema -> Bool
isNamedType s = case s of
  NamedType _ -> True
  _ -> False

isRecord :: Schema -> Bool
isRecord s = case s of
  Record {} -> True
  _ -> False