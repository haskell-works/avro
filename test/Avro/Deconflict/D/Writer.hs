{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Deconflict.D.Writer where

import Data.Avro.Deriving
import Data.Avro.Sum

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
            { "name": "fieldA", "type": "int" }
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
  Foo (makeNSum (Baz (Foo (makeNSum (Baz (Foo (makeNSum $ Bar 12)))))))
