{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Avro.NormSchemaSpec
where

import           Data.Avro.Schema.Schema (Schema (..), fields, fldType, mkUnion)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.Set                as S
import qualified Data.Aeson              as Aeson
import           Avro.Data.Karma
import           Avro.Data.Reused
import           Avro.Data.TwoBits

import Test.Hspec

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Avro.NormSchemaSpec" $ do
  it "should have one full inner schema for each type" $
    (fldType <$> fields schema'ContainerChild) `shouldBe` [schema'ReusedChild, NamedType "Boo.ReusedChild"]

  it "should normalise schemas from unions" $
     fldType <$> fields schema'Curse `shouldBe` [mkUnion (Null :| [schema'Geo])]

  it "should serialise reused schema correctly" $
    let Just expected = Aeson.encode <$> Aeson.decode @Schema twoBits'rawSchema
    in Aeson.encode schema'TwoBits `shouldBe` expected
