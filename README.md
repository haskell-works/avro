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

## Quickstart

This library provides the following conversions between Haskell types and Avro types:

| Haskell type      | Avro type                       |
|:------------------|:--------------------------------|
| ()                | "null"                          |
| Bool              | "boolean"                       |
| Int, Int64        | "long"                          |
| Int32             | "int"                           |
| Double            | "double"                        |
| Text              | "string"                        |
| ByteString        | "bytes"                         |
| Maybe a           | ["null", "a"]                   |
| Either a b        | ["a", "b"]                      |
| Identity a        | ["a"]                           |
| Map Text a        | {"type": "map", "value": "a"}   |
| Map String a      | {"type": "map", "value": "a"}   |
| HashMap Text a    | {"type": "map", "value": "a"}   |
| HashMap String a  | {"type": "map", "value": "a"}   |
| [a]               | {"type": "array", "value": "a"} |

User defined data types should provide `HasAvroSchema`/`ToAvro`/`FromAvro` instances to be encoded/decoded to/from Avro.

### Generating code from Avro schema

Typically these imports are useful:
```haskell
import           Data.Avro
import           Data.Avro.Deriving (deriveAvroFromByteString, r)
```

Now `deriveAvroFromByteString` can be used to generate Haskell types and instances from a given Avro schema.

```haskell
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

As well as all the useful instances for these types: `Eq`, `Show`, `Generic`, `NFData`, `DecodeAvro`, `EncodeAvro`.

See `Data.Avro.Deriving` module for more options like code generation from Avro schemas in files, specifying strictness and prefixes, etc.

