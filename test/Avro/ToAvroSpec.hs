{-# LANGUAGE OverloadedStrings #-}
module Avro.ToAvroSpec
where

import           Data.Avro
import           Data.Int
import           Data.Text
import           Data.Avro.Schema
import qualified Data.Avro.Types as AT
import           Data.List.NonEmpty (NonEmpty(..))

import Test.Hspec

data TypesTestMessage = TypesTestMessage
  { tmId          :: Int64
  , tmName        :: Text
  , tmTimestamp   :: Maybe Int64
  , tmForeignId   :: Maybe Int64
  } deriving (Show, Eq)

tmSchema :: Schema
tmSchema =
  let fld nm = Field nm [] Nothing Nothing
   in Record "TypesTestMessage" (Just "avro.haskell.test") [] Nothing Nothing
        [ fld "id" Long Nothing
        , fld "name" String Nothing
        , fld "timestamp" (mkUnion (Null :| [Long])) Nothing
        , fld "foreignId" (mkUnion (Null :| [Long])) Nothing
        ]

instance ToAvro TypesTestMessage where
  toAvro m = record tmSchema
    [ "id"          .= tmId m
    , "name"        .= tmName m
    , "timestamp"   .= tmTimestamp m
    , "foreignId"   .= tmForeignId m
    ]
  schema = pure tmSchema

instance FromAvro TypesTestMessage where
  fromAvro (AT.Record _ r) =
    TypesTestMessage <$> r .: "id"
                     <*> r .: "name"
                     <*> r .: "timestamp"
                     <*> r .: "foreignId"
  fromAvro v = badValue v "TypesTestMessage"

message :: TypesTestMessage
message = TypesTestMessage
  { tmId         = 896543
  , tmName       = "test-name"
  , tmTimestamp  = Just 7
  , tmForeignId  = Nothing
  }

spec :: Spec
spec = describe "Kafka.IntegrationSpec" $ do
    it "sends messages to test topic" $ do
      fromAvro (toAvro message) `shouldBe` pure message
