{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Data.Deconflict.Read where

import Data.Avro.Deriving
import Text.RawString.QQ

deriveAvroFromByteString [r|
[
{ "name": "Foo",
  "type": "record",
  "fields": [
    { "name": "fooBar",
      "type": {
        "name": "Bar",
        "type": "record",
        "fields": [
          { "name": "barInt",     "type": "int" },
          { "name": "barTime",    "type": { "logicalType": "timestamp-millis", "type": "long" } },
          { "name": "barLong",    "type": { "logicalType": "timestamp-micros", "type": "long" } },
          { "name": "barString",  "type": "string" },
          { "name": "barMissing", "type": "double", "default": 42.2}
        ]
      }
    },
    { "name": "fooOption", "type": ["string", "null"], "default": "default value" },
    { "name": "fooUnion",  "type": ["string", "int", "float"] }
  ]
}
]
|]

--           { "name": "barLong",    "type": { "logicalType": "timestamp-micros", "type": "long" } },
