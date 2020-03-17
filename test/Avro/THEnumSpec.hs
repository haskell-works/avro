{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THEnumSpec
where

import Avro.TestUtils     (roundtrip)
import Data.Avro.Deriving

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
deriveAvroWithOptions (defaultDeriveOptions { namespaceBehavior = HandleNamespaces }) "test/data/enums.avsc"

spec :: Spec
spec = describe "Avro.THEnumSpec: Schema with enums" $ do
  it "should do roundtrip" $ do
    let msg = Haskell'avro'example'EnumWrapper
              { haskell'avro'example'EnumWrapperId   = 42
              , haskell'avro'example'EnumWrapperName = "Text"
              , haskell'avro'example'EnumWrapperReason = Haskell'avro'example'EnumReasonBecause
              }
    roundtrip schema'haskell'avro'example'EnumWrapper msg `shouldBe` pure msg
