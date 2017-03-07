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
  , tmAttraction  :: Double
  } deriving (Show, Eq)

tmSchema :: Schema
tmSchema =
  let fld nm = Field nm [] Nothing Nothing
   in Record "TypesTestMessage" (Just "avro.haskell.test") [] Nothing Nothing
        [ fld "id" Long Nothing
        , fld "name" String Nothing
        , fld "timestamp" (mkUnion (Null :| [Long])) Nothing
        , fld "foreignId" (mkUnion (Null :| [Long])) Nothing
        , fld "competence" (mkUnion (Null :| [Double])) Nothing
        , fld "attraction" Double Nothing
        ]

instance ToAvro TypesTestMessage where
  toAvro m = record tmSchema
    [ "id"          .= tmId m
    , "name"        .= tmName m
    , "timestamp"   .= tmTimestamp m
    , "foreignId"   .= tmForeignId m
    , "competence"  .= tmCompetence m
    , "attraction"  .= tmAttraction m
    ]
  schema = pure tmSchema

instance FromAvro TypesTestMessage where
  fromAvro (AT.Record _ r) =
    TypesTestMessage <$> r .: "id"
                     <*> r .: "name"
                     <*> r .: "timestamp"
                     <*> r .: "foreignId"
                     <*> r .: "competence"
                     <*> r .: "attraction"
  fromAvro v = badValue v "TypesTestMessage"

message :: TypesTestMessage
message = TypesTestMessage
  { tmId         = 896543
  , tmName       = "test-name"
  , tmTimestamp  = Just 7
  , tmForeignId  = Nothing
  , tmCompetence = Just 7.5
  , tmAttraction = 8.974
  }

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

exampleOnlyInt64Value :: OnlyInt64
exampleOnlyInt64Value = OnlyInt64
  { onlyInt64Value    = minBound
  }

spec :: Spec
spec = describe "Kafka.IntegrationSpec" $ do
    -- it "sends messages to test topic" $ do
    --   let x = untag (schema :: Tagged OnlyInt64 Type)
    --   decode x (encode exampleOnlyInt64Value) `shouldBe` Success exampleOnlyInt64Value
    -- it "foo" $ do
    --   let expectedBuffer = BL.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
    --   let value = OnlyInt64 90071992547409917
    --   encode value `shouldBe` expectedBuffer
    it "property" $ do
      Q.property $ \(w :: Int64) ->
        let x = untag (schema :: Tagged OnlyInt64 Type) in
          decode x (encode (OnlyInt64 w)) == Success (OnlyInt64 w)
