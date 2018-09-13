{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.SchemaSpec
where

import           Data.Avro
import           Data.Avro.Deriving (makeSchema)
import           Data.Avro.Schema   (overlay, matches)

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.SchemaSpec" $ do
  describe "overlay" $
    it "should support merging multiple schemas" $ do
      let expected   = $(makeSchema "test/data/overlay/expectation.avsc")
          consumer   = $(makeSchema "test/data/overlay/composite.avsc")
          primitives = $(makeSchema "test/data/overlay/primitives.avsc")

      consumer `overlay` primitives `shouldSatisfy` matches expected