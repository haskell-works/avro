{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.DoubleSpec (spec) where

import           Data.Avro
import           Data.Avro.Schema
import           Data.Tagged
import           Test.Hspec
import qualified Data.Avro.Types      as AT
import qualified Data.ByteString.Lazy as BL
import qualified Test.QuickCheck      as Q

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

newtype OnlyDouble = OnlyDouble
  {onlyDoubleValue :: Double
  } deriving (Show, Eq)

onlyDoubleSchema :: Schema
onlyDoubleSchema =
  let fld nm = Field nm [] Nothing Nothing
  in Record "OnlyDouble" (Just "test.contract") [] Nothing Nothing
        [ fld "onlyDoubleValue" Double Nothing
        ]

instance ToAvro OnlyDouble where
  toAvro sa = record onlyDoubleSchema
    [ "onlyDoubleValue" .= onlyDoubleValue sa ]
  schema = pure onlyDoubleSchema

instance FromAvro OnlyDouble where
  fromAvro (AT.Record _ r) =
    OnlyDouble <$> r .: "onlyDoubleValue"

spec :: Spec
spec = describe "Avro.Codec.DoubleSpec" $ do
  it "Can decode 0.89" $ do
    let expectedBuffer = BL.pack [123, 20, -82, 71, -31, 122, -20, 63]
    let value = OnlyDouble 0.89
    encode value `shouldBe` expectedBuffer

  it "Can decode -2.0" $ do
    let expectedBuffer = BL.pack [0, 0, 0, 0, 0, 0, 0, -64]
    let value = OnlyDouble (-2.0)
    encode value `shouldBe` expectedBuffer

  it "Can decode 1.0" $ do
    let expectedBuffer = [0, 0, 0, 0, 0, 0, -16, 63]
    let value = OnlyDouble 1.0
    BL.unpack (encode value) `shouldBe` expectedBuffer

  it "Can decode encoded Double values" $ do
    Q.property $ \(d :: Double) ->
      let x = untag (schema :: Tagged OnlyDouble Type) in
        decode x (encode (OnlyDouble d)) == Success (OnlyDouble d)
