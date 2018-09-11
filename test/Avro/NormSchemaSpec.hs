{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.NormSchemaSpec
where

import           Data.Avro
import           Data.Avro.Deriving
import           Data.Avro.Schema   (Type (..), fields, fldType, mkUnion)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set           as S

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/reused.avsc"

deriveAvro "test/data/karma.avsc"

spec :: Spec
spec = describe "Avro.NormSchemaSpec" $ do
  it "should have one full inner schema for each type" $
    (fldType <$> fields schema'ContainerChild) `shouldBe` [schema'ReusedChild, NamedType "Boo.ReusedChild"]

  it "should normalise schemas from unions" $
     fldType <$> fields schema'Curse `shouldBe` [mkUnion (Null :| [schema'Geo])]
