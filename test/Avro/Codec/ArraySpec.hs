{-# LANGUAGE ScopedTypeVariables #-}
module Avro.Codec.ArraySpec (spec) where

import           Data.Avro.Schema.ReadSchema (fromSchema)
import qualified Data.Avro.Schema.Schema     as Schema
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U

import           Avro.TestUtils
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Hedgehog.Range              (Range)
import qualified Hedgehog.Range              as Range
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.Codec.ArraySpec" $ do
  it "list roundtip" $ require $ property $ do
    let schema   = Schema.Array (Schema.Int Nothing)
    let arrayGen = Gen.list (Range.linear 0 255) (Gen.int32 (Range.linearBounded))
    roundtripGen schema arrayGen

  it "map roundtrip" $ require $ property $ do
    let schema = Schema.Map (Schema.Long Nothing)
    let keyGen = Gen.text (Range.linear 0 64) Gen.alphaNum
    let valueGen = Gen.int64 Range.linearBounded
    let kvGen = (,) <$> keyGen <*> valueGen
    roundtripGen schema (Gen.map (Range.linear 0 15) kvGen)


  it "vector roundtrip" $ require $ property $ do
    let schema   = Schema.Array (Schema.Int Nothing)
    let arrayGen = Gen.list (Range.linear 0 255) (Gen.int32 (Range.linearBounded))
    roundtripGen schema (V.fromList <$> arrayGen)

  it "unboxed vector roundtrip" $ require $ property $ do
    let schema   = Schema.Array (Schema.Int Nothing)
    let arrayGen = Gen.list (Range.linear 0 255) (Gen.int32 (Range.linearBounded))
    roundtripGen schema (U.fromList <$> arrayGen)
