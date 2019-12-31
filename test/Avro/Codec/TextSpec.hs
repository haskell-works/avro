{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.TextSpec (spec) where

import           Data.Avro
import           Data.Avro.Schema
import           Data.Text
import qualified Data.ByteString.Lazy as BSL
import           Data.Tagged
import           Test.Hspec
import qualified Data.Avro.Types      as AT
import qualified Test.QuickCheck      as Q

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

newtype OnlyText = OnlyText
  {onlyTextValue :: Text
  } deriving (Show, Eq)

onlyTextSchema :: Schema
onlyTextSchema =
  let fld nm = Field nm [] Nothing Nothing False
  in Record "test.contract.OnlyText" [] Nothing Nothing
        [ fld "onlyTextValue" String Nothing
        ]

instance HasAvroSchema OnlyText where
  schema = pure onlyTextSchema

instance ToAvro OnlyText where
  toAvro sa = record onlyTextSchema
    [ "onlyTextValue" .= onlyTextValue sa ]

instance FromAvro OnlyText where
  fromAvro (AT.Record _ r) =
    OnlyText <$> r .: "onlyTextValue"

spec :: Spec
spec = describe "Avro.Codec.TextSpec" $ do
  it "Can decode \"This is an unit test\"" $ do
    -- The '(' here is the length (ASCII value) of the string
    let expectedBuffer = "(This is an unit test"
    let value = OnlyText "This is an unit test"
    encode value `shouldBe` expectedBuffer

  it "Can decode encoded Text values" $ do
    Q.property $ \(t :: String) ->
      decode (encode (OnlyText (pack t))) == Success (OnlyText (pack t))

  it "Can process corrupted Text values without crashing" $ do
    Q.property $ \bytes ->
      let result                   = decode (BSL.pack bytes) :: Result Text
          isSafeResult (Success _) = True
          isSafeResult (Error _)   = True
      in result `shouldSatisfy` isSafeResult
