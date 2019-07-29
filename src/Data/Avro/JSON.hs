{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Avro supports a JSON representation of Avro objects alongside the
-- Avro binary format. An Avro schema can be used to generate and
-- validate JSON representations of Avro objects.
--
-- The JSON format is the same format as used for default values in
-- schemas except unions are encoded differently. Non-union values are
-- encoded as follows:
--
-- +--------------+----------+----------+
-- |Avro Type     |JSON Type |Example   |
-- +==============+==========+==========+
-- |null          |null      |null      |
-- +--------------+----------+----------+
-- |boolean       |boolean   |true      |
-- +--------------+----------+----------+
-- |int, long     |integer   |1         |
-- +--------------+----------+----------+
-- |float, double |number    |1.1       |
-- +--------------+----------+----------+
-- |bytes         |string    |"\u00FF"  |
-- +--------------+----------+----------+
-- |string        |string    |"foo"     |
-- +--------------+----------+----------+
-- |record        |object    |{"a":1}   |
-- +--------------+----------+----------+
-- |enum          |string    |"FOO"     |
-- +--------------+----------+----------+
-- |array         |array     |[1]       |
-- +--------------+----------+----------+
-- |map           |object    |{"a":1}   |
-- +--------------+----------+----------+
-- |fixed         |string    |"\u00FF"  |
-- +--------------+----------+----------+
--
-- (Table from the Avro 1.8.2 specification:
-- <https://avro.apache.org/docs/1.8.2/spec.html#schema_record>)
--
-- Bytes and fixed are encoded as JSON strings where each byte is
-- translated into the corresponding Unicode codepoint between 0–255,
-- which includes non-printable characters. Note that this encoding
-- happens at the Unicode code-point level, meaning it is independent
-- of text encoding. (JSON is, by definition, encoded in UTF8.)
--
-- Unions are encoded as an object with a single field that specifies
-- the "branch" of the union. If the branch is a primitive type like
-- @"string"@, the name of the primitive type is used:
--
-- @
-- { "string" : "foo" }
-- @
--
-- For named types (record, enum and fixed), the name of the type is
-- used:
--
-- @
-- { "MyRecord" : { ... } }
-- @
module Data.Avro.JSON where

import Data.Semigroup ((<>))

import qualified Data.Aeson           as Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  ((!))
import qualified Data.HashMap.Strict  as HashMap
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Tagged
import qualified Data.Text            as Text

import           Data.Avro        (FromAvro (..), Result (..), ToAvro (..))
import qualified Data.Avro        as Avro
import           Data.Avro.Schema (Schema, parseAvroJSON)
import qualified Data.Avro.Schema as Schema
import qualified Data.Avro.Types  as Avro
import qualified Data.Vector      as V

decodeAvroJSON :: Schema -> Aeson.Value -> Result (Avro.Value Schema)
decodeAvroJSON schema json =
  parseAvroJSON union env schema json
  where
    env =
      Schema.buildTypeEnvironment missing schema
    missing name =
      fail ("Type " <> show name <> " not in schema")

    union (Schema.Union schemas) Aeson.Null
      | Schema.Null `elem` schemas =
          pure $ Avro.Union schemas Schema.Null Avro.Null
      | otherwise                  =
          fail "Null not in union."
    union (Schema.Union schemas) (Aeson.Object obj)
      | null obj =
          fail "Invalid encoding of union: empty object ({})."
      | length obj > 1 =
          fail "Invalid encoding of union: object with too many fields."
      | otherwise      =
          let
            canonicalize name
              | isBuiltIn name = name
              | otherwise      = Schema.renderFullname $ Schema.parseFullname name
            branch =
              head $ HashMap.keys obj
            names =
              HashMap.fromList [(Schema.typeName t, t) | t <- V.toList schemas]
          in case HashMap.lookup (canonicalize branch) names of
            Just t  -> do
              nested <- parseAvroJSON union env t (obj ! branch)
              return (Avro.Union schemas t nested)
            Nothing -> fail ("Type '" <> Text.unpack branch <> "' not in union: " <> show schemas)
    union Schema.Union{} _ =
      Avro.Error "Invalid JSON representation for union: has to be a JSON object with exactly one field."
    union _ _ =
      error "Impossible: function given non-union schema."

    isBuiltIn name = name `elem` [ "null", "boolean", "int", "long", "float"
                                 , "double", "bytes", "string", "array", "map" ]

-- | Convert a 'Aeson.Value' into a type that has an Avro schema. The
-- schema is used to validate the JSON and will return an 'Error' if
-- the JSON object is not encoded correctly or does not match the schema.
fromJSON :: forall a. (FromAvro a) => Aeson.Value -> Result a
fromJSON json = do
  value <- decodeAvroJSON schema json
  fromAvro value
  where
    schema = untag (Avro.schema :: Tagged a Schema)

-- | Parse a 'ByteString' as JSON and convert it to a type with an
-- Avro schema. Will return 'Error' if the input is not valid JSON or
-- the JSON does not convert with the specified schema.
parseJSON :: forall a. (FromAvro a) => ByteString -> Result a
parseJSON input = case Aeson.eitherDecode input of
  Left msg    -> Error msg
  Right value -> fromJSON value

-- | Convert an object with an Avro schema to JSON using that schema.
--
-- We always need the schema to /encode/ to JSON because representing
-- unions requires using the names of named types.
toJSON :: forall a. (ToAvro a) => a -> Aeson.Value
toJSON = Aeson.toJSON . toAvro
