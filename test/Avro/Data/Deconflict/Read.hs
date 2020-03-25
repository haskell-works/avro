{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Data.Deconflict.Read where

import Data.Avro.Deriving

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
          { "name": "barMissing", "type": "double", "default": 42.2},
          { "name": "barMooMissing",
            "type": {
              "type": "record",
              "name": "Moo",
              "fields": [
                { "name": "mooInt", "type": "int"},
                { "name": "mooLong", "type": "long"}
              ]
            },
            "default": { "mooLong": 2, "mooInt": 42 }
          }
        ]
      }
    },
    { "name": "fooOption", "type": ["string", "null"], "default": "default value" },
    { "name": "fooUnion",  "type": ["string", "int", "float"] }
  ]
}
]
|]
