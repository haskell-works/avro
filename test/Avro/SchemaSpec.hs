{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.SchemaSpec
where

import           Data.Avro
import           Data.Avro.Deriving (makeSchema)
import           Data.Avro.Schema   (buildTypeEnvironment, matches)

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.SchemaSpec" $ do
  describe "buildTypeEnvironment" $
    it "should contain definitions for all internal types" $ do
      let schema      = $(makeSchema "test/data/internal-bindings.avsc")
          environment = buildTypeEnvironment err schema
          err name    = fail $ "Missing " <> show name <> " in environment."
          expected    =
            [ "InternalBindings"
            , "InField"
            , "NestedInField"
            , "AliasNestedInField"
            , "NestedEnum"
            , "NestedFixed"
            , "InArray"
            , "NestedInArray"
            , "InMap"
            , "NestedInMap"
            , "InUnionA"
            , "InUnionB"
            ]
      definitions <- traverse environment expected
      length definitions `shouldBe` length expected
