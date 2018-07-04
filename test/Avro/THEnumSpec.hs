{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THEnumSpec
where

import           Data.Avro
import           Data.Avro.Deriving

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/enums.avsc"

spec :: Spec
spec = describe "Avro.THEnumSpec: Schema with enums" $ do
  it "should do roundtrip" $ do
    let msg = EnumWrapper
              { enumWrapperId   = 42
              , enumWrapperName = "Text"
              , enumWrapperReason = EnumReasonBecause
              }
    fromAvro (toAvro msg) `shouldBe` pure msg
