# Native Haskell implementation of Avro

[![CircleCI](https://circleci.com/gh/haskell-works/avro.svg?style=svg)](https://circleci.com/gh/haskell-works/avro)
[![Hackage](https://img.shields.io/hackage/v/avro.svg?logo=haskell)](https://hackage.haskell.org/package/avro)

This is a Haskell [Avro](https://avro.apache.org/) library useful for decoding
and encoding Avro data structures.  Avro can be thought of as a serialization
format and RPC specification which induces three separable tasks:

* *Serialization*/*Deserialization* - This library has been used "in anger" for:
  - Deserialization of avro container files
  - Serialization/deserialization Avro messages to/from Kafka topics
* *RPC* - There is currently no support for Avro RPC in this library.

This library also provides functionality for automatically generating Avro-related data types and instances from Avro schemas (using TemplateHaskell).

# Quickstart

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

## Defining types and `HasAvroSchema` / `FromAvro` / `ToAvro` manually

Typically these imports are useful:
```
import           Data.Avro
import           Data.Avro.Schema as S
import qualified Data.Avro.Types  as AT
```

Assuming there is a data type to be encoded/decoded from/to Avro:
```
data Gender = Male | Female deriving (Eq, Ord, Show, Enum)
data Person = Person
     { fullName :: Text
     , age      :: Int32
     , gender   :: Gender
     , ssn      :: Maybe Text
     } deriving (Show, Eq)
```

Avro schema for this type can be defined as:
```
genderSchema :: Schema
genderSchema = mkEnum "Gender" [] Nothing Nothing ["Male", "Female"]

personSchema :: Schema
personSchema =
  Record "Person" Nothing [] Nothing Nothing
    [ fld "name"   String       Nothing
    , fld "age"    Int          Nothing
    , fld "gender" genderSchema Nothing
    , fld "ssn" (mkUnion $ Null :| [String]) Nothing
    ]
    where
     fld nm ty def = Field nm [] Nothing Nothing ty def

instance HasAvroSchema Person where
  schema = pure personSchema
```

`ToAvro` instance for `Person` can be defined as:
```
instance ToAvro Person where
  schema = pure personSchema
  toAvro p = record personSchema
             [ "name"   .= fullName p
             , "age"    .= age p
             , "gender" .= gender p
             , "ssn"    .= ssn p
             ]
```

`FromAvro` instance for `Person` can be defined as:
```
instance FromAvro Person where
  fromAvro (AT.Record _ r) =
    Person <$> r .: "name"
           <*> r .: "age"
           <*> r .: "gender"
           <*> r .: "ssn"
  fromAvro r = badValue r "Person"
```

## Defining types and `HasAvroSchema` / `FromAvro` / `ToAvro` "automatically"
This library provides functionality to derive Haskell data types and `HasAvroSchema`/`FromAvro`/`ToAvro` instances "automatically" from already existing Avro schemas (using TemplateHaskell).

### Examples

`deriveAvro` will derive data types, `FromAvro` and `ToAvro` instances from a provided Avro schema file:
```
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
import Data.Avro.Deriving

deriveAvro "schemas/contract.avsc"
```

Similarly, `deriveFromAvro` can be used to only derive data types and `FromAvro`, but not `ToAvro` instances.

If you prefer defining Avro schema in Haskell and not in `avsc`, then `deriveAvro'` can be used instead of `deriveAvro`.

### Conventions
When Haskell data types are generated, these conventions are followed:

- Type and field names are "sanitized":
all the charachers except `[a-z,A-Z,',_]` are removed from names
- Field names are prefixed with the name of the record they are declared in.

For example, if Avro schema defines `Person` record as:
```
{ "type": "record",
  "name": "Person",
  "fields": [
    { "name": "name", "type": "string"}
  ]
}
```

then generated Haskell type will look like:
```
data Person = Person
     { personName :: Text
     } deriving (Show, Eq)
```

### Limitations
Two-parts unions like `["null", "MyType"]` or `["MyType", "YourType"]` are supported (as Haskell's `Maybe MyType` and `Either MyType YourType`), but multi-parts unions are currently _not_ supported.
It is not due to any fundamental problems but because it has not been done yet. PRs are welcomed! :)
# TODO
Please see the [TODO](TODO)
