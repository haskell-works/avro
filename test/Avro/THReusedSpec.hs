{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THReusedSpec
where

import Avro.TestUtils     (roundtrip)
import Data.Avro.Deriving

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvroWithOptions (defaultDeriveOptions { namespaceBehavior = HandleNamespaces }) "test/data/reused.avsc"

spec :: Spec
spec = describe "Avro.THReusedSpec: Schema with named types" $ do
  let container = Boo'ContainerChild
                  { boo'ContainerChildFstIncluded = Boo'ReusedChild 100
                  , boo'ContainerChildSndIncluded = Boo'ReusedChild 200
                  }
  let wrapper = Boo'ReusedWrapper
                { boo'ReusedWrapperFull  = Boo'ReusedChild 42
                , boo'ReusedWrapperInner = container
                }
  it "wrapper should do roundtrip" $
    roundtrip schema'Boo'ReusedWrapper wrapper `shouldBe` pure wrapper

  it "child should do rundtrip" $
    roundtrip schema'Boo'ContainerChild container `shouldBe` pure container

  it "innermost element should do roundtrip" $
    let child = Boo'ReusedChild 7
    in roundtrip schema'Boo'ReusedChild child `shouldBe` pure child


