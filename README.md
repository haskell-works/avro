# Native Haskell implementation of Avro

[![CircleCI](https://circleci.com/gh/haskell-works/avro.svg?style=svg)](https://circleci.com/gh/haskell-works/avro)
[![Hackage](https://img.shields.io/hackage/v/avro.svg?logo=haskell)](https://hackage.haskell.org/package/avro)

This is a Haskell [Avro](https://avro.apache.org/) library useful for decoding
and encoding Avro data structures.  Avro can be thought of as a serialization
format and RPC specification which induces three separable tasks:

* *Serialization*/*Deserialization* - This library has been used "in anger" for:
  * Deserialization of avro container files
  * Serialization/deserialization Avro messages to/from Kafka topics
* *RPC* - There is currently no support for Avro RPC in this library.

## Generating code from Avro schema

The preferred method to use Avro is to be "schema first". </br>
This library supports this idea by providing the ability to generate all the necessary entries (types, class instances, etc.) from Avro schemas.

```haskell
import Data.Avro
import Data.Avro.Deriving (deriveAvroFromByteString, r)

deriveAvroFromByteString [r|
{
  "name": "Person",
  "type": "record",
  "fields": [
    { "name": "fullName", "type": "string" },
    { "name": "age", "type": "int" },
    { "name": "gender",
      "type": { "type": "enum", "symbols": ["Male", "Female"] }
    },
    { "name": "ssn", "type": ["null", "string"] }
  ]
}
|]
```

This code will generate the following entries:

```haskell
data Gender = GenderMale | GenderFemale

schema'Gender :: Schema
schema'Gender = ...

data Person = Person
  { personFullName  :: Text
  , personAge       :: Int32
  , personGender    :: Gender,
  , personSsn       :: Maybe Text
  }

schema'Person :: Schema
schema'Person = ...
```

As well as all the useful instances for these types: `Eq`, `Show`, `Generic`, noticing `HasAvroSchema`, `FromAvro` and `ToAvro`.

See `Data.Avro.Deriving` module for more options like code generation from Avro schemas in files, specifying strictness and prefixes, etc.

## Using Avro with existing Haskell types

**Note**: This is an advanced topic. Prefer generating from schemas unless it is required to make Avro work with manually defined Haskell types.

In this section we assume that the following Haskell type is manually defined:

```haskell
data Person = Person
  { fullName  :: Text
  , age       :: Int32
  , ssn       :: Maybe Text
  } deriving (Eq, Show, Generic)
```

For a Haskell type to be encodable to Avro it should have `ToAvro` instance, and to be decodable from Avro it should have `FromAvro` instance.

There is also `HasAvroSchema` class that is useful to have an instance of (although it is not required strictly speaking).


### Creating a schema

A schema can still be generated using TH:

```haskell
schema'Person :: Schema
schema'Person = $(makeSchemaFromByteString [r|
{
  "name": "Person",
  "type": "record",
  "fields": [
    { "name": "fullName", "type": "string" },
    { "name": "age", "type": "int" },
    { "name": "ssn", "type": ["null", "string"] }
  ]
}
|])
```

Alternatively schema can be defined manually:

```haskell
import Data.Avro
import Data.Avro.Schema.Schema (mkUnion)

schema'Person :: Schema
schema'Person =
  Record "Person" []  Nothing                                 Nothing
    [ fld "fullName"  (String Nothing)                        Nothing
    , fld "age"       (Int Nothing)                           Nothing
    , fld "ssn"       (mkUnion $ Null :| [(String Nothing)])  Nothing
    ]
  where
     fld nm ty def = Field nm [] Nothing Nothing ty def
```

---
**NOTE**: When Schema is created separately to a data type there is no way to guarantee that the schema actually matches the type. It will be up to a developer to make sure of that.

Prefer generating data types with `Data.Avro.Deriving` when possible.

---

### Instantiating `FromAvro`

When working with `FromAvro` directly it is important to understand the difference between `Schema` and `ReadSchema`.

`Schema` (as in the example above) is just a regular data schema for an Avro type.

`ReadSchema` is a similar type, but it is capable of captuting and resolving differences between "_writer_ schema" and "_reader_ schema". See [Specification](https://avro.apache.org/docs/current/spec.html#Schema+Resolution) to learn more about schema resolution and de-conflicting.

`FromAvro` class requires `ReaderSchema` because with Avro it is possible to read data with a different schema compared to the schema that was used for writing this data.

`ReadSchema` can be obtained by converting an existing `Schema` with `readSchemaFromSchema` function, or by actually deconflicting two schemas using `deconflict` function.

Another **important fact** is that field's values in Avro payload are written and read _in order_ with how these fields are defined in the schema.

This fact can be exploited in writing `FromAvro` instance for `Person`:

```haskell
import           Data.Avro.Encoding.FromAvro (FromAvro (..))
import qualified Data.Avro.Encoding.FromAvro as FromAvro

instance FromAvro Person where
  fromAvro (FromAvro.Record _schema vs) = Person
    <$> fromAvro (vs Vector.! 0)
    <*> fromAvro (vs Vector.! 1)
    <*> fromAvro (vs Vector.! 2)
```

Fields resolution by name can be performed here (since we have reference to the schema). But in this case it is simpler (and faster) to exploit the fact that the order of values is known and to access required values by their positions.

### Instantiating `ToAvro`

`ToAvro` class is defined as

```haskell
class ToAvro a where
  toAvro :: Schema -> a -> Builder
```

A `Schema` is provided to help with disambiguating how exactly the specified value should be encoded.

For example, `UTCTime` can be encoded as milliseconds or as microseconds depending on schema's _logical type_ accordig to [Specification](https://avro.apache.org/docs/current/spec.html#Logical+Types):

```haskell
instance ToAvro UTCTime where
  toAvro s = case s of
    Long (Just TimestampMicros) ->
      toAvro @Int64 s . fromIntegral . utcTimeToMicros

    Long (Just TimestampMillis)) ->
      toAvro @Int64 s . fromIntegral . utcTimeToMillis
```

`ToAvro` instance for `Person` data type from the above could look like:

```haskell
import Data.Avro.Encoding.ToAvro (ToAvro(..), record, ((.=)))

instance ToAvro Person where
  toAvro schema value =
    record schema
      [ "fullName"  .= fullName value
      , "age"       .= age value
      , "ssn"       .= ssn value
      ]
```

`record` helper function is responsible for propagaing individual fields' schemas (found in the provided `schema`) when `toAvro`'ing nested values.

## Type mapping

Full list can be found in `ToAvro` and `FromAvro` modules.

This library provides the following conversions between Haskell types and Avro types:

| Haskell type      | Avro type                                               |
|:------------------|:--------------------------------------------------------|
| ()                | "null"                                                  |
| Bool              | "boolean"                                               |
| Int, Int64        | "long"                                                  |
| Int32             | "int"                                                   |
| Double            | "double"                                                |
| Text              | "string"                                                |
| ByteString        | "bytes"                                                 |
| Maybe a           | ["null", "a"]                                           |
| Either a b        | ["a", "b"]                                              |
| Identity a        | ["a"]                                                   |
| Map Text a        | { "type": "map",    "value": "a" }                      |
| Map String a      | { "type": "map",    "value": "a" }                      |
| HashMap Text a    | { "type": "map",    "value": "a" }                      |
| HashMap String a  | { "type": "map",    "value": "a" }                      |
| [a]               | { "type": "array",  "value": "a" }                      |
| UTCTime           | { "type": "long",   "logicalType": "timestamp-millis" } |
| UTCTime           | { "type": "long",   "logicalType": "timestamp-micros" } |
| DiffTime          | { "type": "int",    "logicalType": "time-millis" }      |
| DiffTime          | { "type": "long",   "logicalType": "time-micros" }      |
| Day               | { "type": "int",    "logicalType": "date" }             |
| UUID              | { "type": "string", "logicalType": "uuid" }             |

User defined data types should provide `HasAvroSchema` / `ToAvro` / `FromAvro` instances to be encoded/decoded to/from Avro.
