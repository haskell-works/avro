{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THReusedSpec
where

import           Data.Avro
import           Data.Avro.Deriving

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/reused.avsc"

spec :: Spec
spec = describe "Avro.THReusedSpec: Schema with named types" $ do
  let container = ContainerChild
                  { containerChildFstIncluded = ReusedChild 100
                  , containerChildSndIncluded = ReusedChild 200
                  }
  let wrapper = ReusedWrapper
                { reusedWrapperFull  = ReusedChild 42
                , reusedWrapperInner = container
                }
  it "wrapper should do roundtrip" $
    fromAvro (toAvro wrapper)         `shouldBe` pure wrapper

  it "child should do rundtrip" $
    fromAvro (toAvro container)       `shouldBe` pure container

  it "innermost element should do roundtrip" $
    fromAvro (toAvro (ReusedChild 7)) `shouldBe` pure (ReusedChild 7)
