{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Avro.ManualSpec
where

import           Data.Avro.Encoding.FromAvro (FromAvro (..))
import qualified Data.Avro.Encoding.FromAvro as FromAvro
import           Data.Avro.Encoding.ToAvro   (ToAvro (..), record, (.=))
import qualified Data.Avro.Schema.ReadSchema as ReadSchema
import           Data.Avro.Schema.Schema
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Int                    (Int32)
import           Data.List.NonEmpty          (NonEmpty ((:|)))
import           Data.Text                   (Text)
import qualified Data.Vector                 as Vector


import           Avro.TestUtils              (roundtripGen)
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Hedgehog.Range              (Range)
import qualified Hedgehog.Range              as Range
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

data Person = Person
  { fullName :: Text
  , age      :: Int32
  , ssn      :: Maybe Text
  } deriving (Eq, Show)

schema'Person :: Schema
schema'Person =
  Record "Person" []  Nothing                                 Nothing
    [ fld "fullName"  (String Nothing)                        Nothing
    , fld "age"       (Int Nothing)                           Nothing
    , fld "ssn"       (mkUnion $ Null :| [(String Nothing)])  Nothing
    ]
  where
     fld nm ty def = Field nm [] Nothing Nothing ty def

instance ToAvro Person where
  toAvro schema value =
    record schema
      [ "fullName"  .= fullName value
      , "age"       .= age value
      , "ssn"       .= ssn value
      ]


instance FromAvro Person where
  fromAvro (FromAvro.Record _ vs) = Person
    <$> fromAvro (vs Vector.! 0)
    <*> fromAvro (vs Vector.! 1)
    <*> fromAvro (vs Vector.! 2)

personGen :: MonadGen m => m Person
personGen = Person
  <$> Gen.text (Range.linear 0 64) Gen.alphaNum
  <*> Gen.int32 Range.linearBounded
  <*> Gen.maybe (Gen.text (Range.singleton 16) Gen.alphaNum)

spec :: Spec
spec = describe "Avro.ManualSpec" $ do
  it "should roundtrip manually created type" $ require $ property $ do
    roundtripGen schema'Person personGen
