{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.SchemaSpec
where

import qualified Data.HashMap.Lazy  as HashMap
import qualified Data.HashSet       as HashSet

import           Data.Avro
import           Data.Avro.Deriving (makeSchema)
import           Data.Avro.Schema   (extractBindings, matches, overlay)

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.SchemaSpec" $ do
  describe "extractBindings" $
    it "should extract bindings for all internal types" $ do
    let schema = $(makeSchema "test/data/internal-bindings.avsc")
        bindings = extractBindings schema
        expected =
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
    HashSet.fromMap (() <$ bindings) == expected
  describe "overlay" $
    it "should support merging multiple schemas" $ do
      let expected   = $(makeSchema "test/data/overlay/expectation.avsc")
          consumer   = $(makeSchema "test/data/overlay/composite.avsc")
          primitives = $(makeSchema "test/data/overlay/primitives.avsc")

      consumer `overlay` primitives `shouldSatisfy` matches expected
