{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Deconflict.D.Writer where

import Data.Avro.Deriving

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
  Foo (Right (Baz (Foo (Right (Baz (Foo (Left $ Bar 12)))))))
