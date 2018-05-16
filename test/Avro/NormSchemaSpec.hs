{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.NormSchemaSpec
where

import           Data.Avro
import           Data.Avro.Deriving
import           Data.Avro.Schema   (Type (..), fields, fldType)
import qualified Data.Set           as S

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/reused.avsc"

spec :: Spec
spec = describe "Avro.NormSchemaSpec" $ do
  it "should have one full inner schema for each type" $
    (fldType <$> fields schema'ContainerChild) `shouldBe` [schema'ReusedChild, NamedType "ReusedChild"]
