{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.Int64Spec (spec) where

import           Data.Avro
import           Data.Avro.Schema
import           Data.Int
import           Data.Tagged
import           Test.Hspec
import qualified Data.Avro.Types      as AT
import qualified Data.ByteString.Lazy as BL
import qualified Test.QuickCheck      as Q

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

newtype OnlyInt64 = OnlyInt64
  { onlyInt64Value :: Int64
  } deriving (Show, Eq)

onlyInt64Schema :: Schema
onlyInt64Schema =
  let fld nm = Field nm [] Nothing Nothing
   in Record "OnlyInt64" (Just "test.contract") [] Nothing Nothing
        [ fld "onlyInt64Value"    Long Nothing
        ]

instance ToAvro OnlyInt64 where
  toAvro sa = record onlyInt64Schema
    [ "onlyInt64Value" .= onlyInt64Value sa
    ]
  schema = pure onlyInt64Schema

instance FromAvro OnlyInt64 where
  fromAvro (AT.Record _ r) =
    OnlyInt64  <$> r .: "onlyInt64Value"

spec :: Spec
spec = describe "Avro.Codec.Int64Spec" $ do
  it "Can encode 90071992547409917L correctly" $ do
    let expectedBuffer = BL.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
    let value = OnlyInt64 90071992547409917
    encode value `shouldBe` expectedBuffer
  -- it "Can encode 90071992547409917L correctly" $ do
  --   let expectedBuffer = BL.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
  --   let value = OnlyInt64 4611686018427387904
  --   print (encode value)
  --   encode value `shouldBe` expectedBuffer
  it "Can decode encoded Int64 values" $ do
    Q.property $ \(w :: Int64) ->
      let x = untag (schema :: Tagged OnlyInt64 Type) in
        decode x (encode (OnlyInt64 w)) == Success (OnlyInt64 w)
