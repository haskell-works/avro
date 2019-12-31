{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.NestedSpec (spec) where

import           Data.Avro
import           Data.Avro.Schema
import qualified Data.Avro.Types      as AT
import qualified Data.ByteString.Lazy as BL
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

data ChildType = ChildType
  { childValue1 :: Int
  , childValue2 :: Int
  } deriving (Show, Eq)

data ParentType = ParentType
  { parentValue1 :: Int
  , parentValue2 :: [ChildType]
  } deriving (Show, Eq)

childTypeSchema :: Schema
childTypeSchema =
  let fld nm = Field nm [] Nothing Nothing False
  in Record "test.contract.ChildType" [] Nothing Nothing
        [ fld "childValue1" Long Nothing
        , fld "childValue2" Long Nothing
        ]

parentTypeSchema :: Schema
parentTypeSchema =
  let fld nm = Field nm [] Nothing Nothing False
  in Record "test.contract.ParentType" [] Nothing Nothing
        [ fld "parentValue1" Long             Nothing
        , fld "parentValue2" (Array childTypeSchema)  Nothing]

instance HasAvroSchema ParentType where
  schema = pure parentTypeSchema

instance HasAvroSchema ChildType where
  schema = pure childTypeSchema

instance ToAvro ChildType where
  toAvro sa = record childTypeSchema
    [ "childValue1" .= childValue1 sa
    , "childValue2" .= childValue2 sa
    ]

instance FromAvro ChildType where
  fromAvro (AT.Record _ r) =
    ChildType <$> r .: "childValue1"
              <*> r .: "childValue2"
  fromAvro v = badValue v "ChildType"

instance ToAvro ParentType where
  toAvro sa = record parentTypeSchema
    [ "parentValue1" .= parentValue1 sa
    , "parentValue2" .= parentValue2 sa
    ]

instance FromAvro ParentType where
  fromAvro (AT.Record _ r) =
    ParentType <$> r .: "parentValue1"
               <*> r .: "parentValue2"
  fromAvro v = badValue v "ParentType"

spec :: Spec
spec = describe "Avro.Codec.NestedSpec" $ do
  it "Can encode/decode nested structures" $ do
    let parent = ParentType 0 [ChildType 1 2, ChildType 3 4]
    let parentEncoded = encode parent

    let parentDecoded = decode parentEncoded
    parentDecoded `shouldBe` Success parent
