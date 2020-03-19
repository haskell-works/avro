{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Deconflict.B.Reader where

import Data.Avro.Deriving

deriveAvroFromByteString [r|
[
{
  "type": "record",
  "name": "Foo",
  "namespace": "avro.test",
  "fields": [
    { "name": "fieldA",
      "type": ["null", {
        "type": "record",
        "name": "Goo",
        "fields": [
          { "name": "fieldB1",
            "type": {
              "type": "record",
              "name": "Moo",
              "fields": [
                { "name": "name",     "type": "string"  },
                { "name": "fullName", "type": ["null", "string"], "default": null }
              ]
            }
          },
          { "name": "fieldB2", "type": "Moo" }
        ]
      }]
    }
  ]
}
]
|]

sampleValue :: Foo
sampleValue = Foo
  { fooFieldA = Just Goo
    { gooFieldB1 = Moo
      { mooName     = "X"
      , mooFullName = Nothing
      }
    , gooFieldB2 = Moo
      { mooName     = "X"
      , mooFullName = Nothing
      }
    }
  }
