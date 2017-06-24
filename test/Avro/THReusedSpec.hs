{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THReusedSpec
where

import           Data.Avro
import           Data.Avro.TH

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/reused.avsc"

spec :: Spec
spec = describe "Avro.THReusedSpec: Schema with named types" $ do
  it "should do roundtrip" $ do
    let msg = ReusedWrapper
              { reusedWrapperFull  = ReusedChild 42
              , reusedWrapperInner = ContainerChild
                                     { containerChildFstIncluded = ReusedChild 100
                                     , containerChildSndIncluded = ReusedChild 200
                                     }
              }
    fromAvro (toAvro msg) `shouldBe` pure msg