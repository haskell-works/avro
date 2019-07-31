{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.ReuseFixedSpec
where

import qualified Data.Aeson           as Aeson
import           Data.Avro            as Avro
import           Data.Avro.Deriving
import           Data.Avro.Schema     (Type (..), fields, fldType, mkUnion)
import           Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.Set             as S

import Test.Hspec

deriveAvro "test/data/fixed-types.avsc"

spec :: Spec
spec = describe "Avro.ReuseFixedSpec" $ do
  it "should generate sensible schema" $ do
    let msg = ReuseFixed (FixedData "ABCDEFGHIJKLMNOP") (FixedData "PONMLKJIHGFEDCBA")
    Avro.decode(Avro.encode(msg)) `shouldBe` Success msg

