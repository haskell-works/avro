{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.ToAvroSpec
where

import           Data.Avro
import           Data.Int
import           Data.Text
import           Data.Avro.Schema
import qualified Data.Avro.Types as AT
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Tagged
import           Data.Word
import qualified Data.ByteString.Lazy as BL
import Test.Hspec
import qualified Test.QuickCheck as Q

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

data TypesTestMessage = TypesTestMessage
  { tmId          :: Int64
  , tmName        :: Text
  , tmTimestamp   :: Maybe Int64
  , tmForeignId   :: Maybe Int64
  , tmCompetence  :: Maybe Double
  , tmRelevance   :: Maybe Float
  , tmSeverity    :: Float
  , tmAttraction  :: Double
  } deriving (Show, Eq)

tmSchema :: Schema
tmSchema =
  let fld nm = Field nm [] Nothing Nothing
   in Record "avro.haskell.test.TypesTestMessage" [] Nothing Nothing
        [ fld "id" Long Nothing
        , fld "name" String Nothing
        , fld "timestamp" (mkUnion (Null :| [Long])) Nothing
        , fld "foreignId" (mkUnion (Null :| [Long])) Nothing
        , fld "competence" (mkUnion (Null :| [Double])) Nothing
        , fld "relevance" (mkUnion (Null :| [Float])) Nothing
        , fld "severity" Float Nothing
        , fld "attraction" Double Nothing
        ]

instance HasAvroSchema TypesTestMessage where
  schema = pure tmSchema

instance ToAvro TypesTestMessage where
  toAvro m = record tmSchema
    [ "id"          .= tmId m
    , "name"        .= tmName m
    , "timestamp"   .= tmTimestamp m
    , "foreignId"   .= tmForeignId m
    , "competence"  .= tmCompetence m
    , "relevance"   .= tmRelevance m
    , "severity"    .= tmSeverity m
    , "attraction"  .= tmAttraction m
    ]

instance FromAvro TypesTestMessage where
  fromAvro (AT.Record _ r) =
    TypesTestMessage <$> r .: "id"
                     <*> r .: "name"
                     <*> r .: "timestamp"
                     <*> r .: "foreignId"
                     <*> r .: "competence"
                     <*> r .: "relevance"
                     <*> r .: "severity"
                     <*> r .: "attraction"
  fromAvro v = badValue v "TypesTestMessage"

message :: TypesTestMessage
message = TypesTestMessage
  { tmId         = 896543
  , tmName       = "test-name"
  , tmTimestamp  = Just 7
  , tmForeignId  = Nothing
  , tmCompetence = Just 7.5
  , tmRelevance  = Just 3.8
  , tmSeverity   = -255.77
  , tmAttraction = 8.974
  }

spec :: Spec
spec = describe "Kafka.IntegrationSpec" $ do
  it "sends messages to test topic" $ do
    fromAvro (toAvro message) `shouldBe` pure message
