{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Deconflict.B.Writer where

import Data.Avro.Deriving
import Text.RawString.QQ

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
                { "name": "name",     "type": "string"  }
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
    { gooFieldB1  = Moo
      { mooName   = "X"
      }
    , gooFieldB2  = Moo
      { mooName   = "X"
      }
    }
  }
