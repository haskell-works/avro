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
  let fld nm = Field nm [] Nothing Nothing False
   in Record "test.contract.onlyMaybeBool" [] Nothing Nothing
        [ fld "onlyMaybeBoolValue" (mkUnion (Null :| [Boolean])) Nothing
        ]

instance HasAvroSchema OnlyMaybeBool where
  schema = pure onlyMaybeBoolSchema

instance ToAvro OnlyMaybeBool where
  toAvro sa = record onlyMaybeBoolSchema
    [ "onlyMaybeBoolValue" .= onlyMaybeBoolValue sa
    ]

instance FromAvro OnlyMaybeBool where
  fromAvro (AT.Record _ r) =
    OnlyMaybeBool <$> r .: "onlyMaybeBoolValue"

spec :: Spec
spec = describe "Avro.Codec.MaybeSpec" $ do
  it "should encode then decode Maybe Bool correctly" $ do
    Q.property $ \(w :: Maybe Bool) ->
      decode (encode (OnlyMaybeBool w)) `shouldBe` Success (OnlyMaybeBool w)
