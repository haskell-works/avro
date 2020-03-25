{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Deconflict.A.Reader where

import Data.Avro.Deriving

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "Outer",
  "fields": [
    { "name": "name", "type": "string" },
    { "name": "inner", "type": {
        "type": "record",
        "name": "Inner",
        "fields": [
          { "name": "id", "type": "int" },
          { "name": "smell", "type": ["null", "string"], "default": null }
        ]
      }
    },
    { "name": "other", "type": "Inner" }
  ]
}
|]

sampleValue :: Outer
sampleValue = Outer "Peone" (Inner 3 Nothing) (Inner 5 Nothing)
