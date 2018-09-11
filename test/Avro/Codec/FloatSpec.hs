{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.FloatSpec (spec) where

import           Data.Avro
import           Data.Avro.Schema
import           Data.Tagged
import           Test.Hspec
import qualified Data.Avro.Types      as AT
import qualified Data.ByteString.Lazy as BL
import qualified Test.QuickCheck      as Q

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

newtype OnlyFloat = OnlyFloat
  {onlyFloatValue :: Float
  } deriving (Show, Eq)

onlyFloatSchema :: Schema
onlyFloatSchema =
  let fld nm = Field nm [] Nothing Nothing
  in Record "test.contract.OnlyFloat" [] Nothing Nothing
        [ fld "onlyFloatValue" Float Nothing
        ]

instance HasAvroSchema OnlyFloat where
  schema = pure onlyFloatSchema

instance ToAvro OnlyFloat where
  toAvro sa = record onlyFloatSchema
    [ "onlyFloatValue" .= onlyFloatValue sa ]

instance FromAvro OnlyFloat where
  fromAvro (AT.Record _ r) =
    OnlyFloat <$> r .: "onlyFloatValue"

spec :: Spec
spec = describe "Avro.Codec.FloatSpec" $ do
  it "Can decode 0.89" $ do
    let expectedBuffer = BL.pack [10, -41, 99, 63]
    let value = OnlyFloat 0.89
    encode value `shouldBe` expectedBuffer

  it "Can decode -2.0" $ do
    let expectedBuffer = BL.pack [0, 0, 0, -64]
    let value = OnlyFloat (-2.0)
    encode value `shouldBe` expectedBuffer

  it "Can decode 1.0" $ do
    let expectedBuffer = [0, 0, 128, 63]
    let value = OnlyFloat 1.0
    BL.unpack (encode value) `shouldBe` expectedBuffer

  it "Can decode encoded Float values" $ do
    Q.property $ \(d :: Float) ->
        decode (encode (OnlyFloat d)) == Success (OnlyFloat d)
