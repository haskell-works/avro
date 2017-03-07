{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.MaybeSpec (spec) where

import           Test.Hspec
import qualified Test.QuickCheck as Q

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Tagged
import           Data.Text

import           Data.Avro
import           Data.Avro.Schema
import qualified Data.Avro.Types as AT

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

newtype OnlyMaybeBool = OnlyMaybeBool
  { onlyMaybeBoolValue :: Maybe Bool
  } deriving (Show, Eq)

onlyMaybeBoolSchema :: Schema
onlyMaybeBoolSchema =
  let fld nm = Field nm [] Nothing Nothing
   in Record "onlyMaybeBool" (Just "test.contract") [] Nothing Nothing
        [ fld "onlyMaybeBoolValue" (mkUnion (Null :| [Boolean])) Nothing
        ]

instance ToAvro OnlyMaybeBool where
  toAvro sa = record onlyMaybeBoolSchema
    [ "onlyMaybeBoolValue" .= onlyMaybeBoolValue sa
    ]
  schema = pure onlyMaybeBoolSchema

instance FromAvro OnlyMaybeBool where
  fromAvro (AT.Record _ r) =
    OnlyMaybeBool <$> r .: "onlyMaybeBoolValue"

spec :: Spec
spec = describe "Avro.Codec.MaybeSpec" $ do
  it "should encode then decode Maybe Bool correctly" $ do
    Q.property $ \(w :: Maybe Bool) ->
      let x = untag (schema :: Tagged OnlyMaybeBool Type) in
        decode x (encode (OnlyMaybeBool w)) `shouldBe` Success (OnlyMaybeBool w)
