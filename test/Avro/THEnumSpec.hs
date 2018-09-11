{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THEnumSpec
where

import           Data.Avro
import           Data.Avro.Deriving

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvroWithNamespaces "test/data/enums.avsc"

spec :: Spec
spec = describe "Avro.THEnumSpec: Schema with enums" $ do
  it "should do roundtrip" $ do
    let msg = Haskell'avro'example'EnumWrapper
              { haskell'avro'example'EnumWrapperId   = 42
              , haskell'avro'example'EnumWrapperName = "Text"
              , haskell'avro'example'EnumWrapperReason = Haskell'avro'example'EnumReasonBecause
              }
    fromAvro (toAvro msg) `shouldBe` pure msg
