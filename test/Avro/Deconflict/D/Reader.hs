{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Deconflict.D.Reader where

import Data.Avro.Deriving
import Text.RawString.QQ

deriveAvroFromByteString [r|
[
{ "name": "Foo",
  "type": "record",
  "fields": [
    { "name": "constructor",
      "type": [
        { "name": "Bar",
          "type": "record",
          "fields": [
            { "name": "fieldA", "type": "int" },
            { "name": "fieldB", "type": [ "null", "string" ], "default": null }
          ]
        },
        { "name": "Baz",
          "type": "record",
          "fields": [
            { "name": "fieldC", "type": "Foo" }
          ]
        }
      ]
    }
  ]
}
]
|]

sampleValue :: Foo
sampleValue =
  Foo (Right (Baz (Foo (Right (Baz (Foo (Left $ Bar 12 Nothing)))))))
