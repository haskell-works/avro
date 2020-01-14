{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Deconflict.Unions.Reader where

import Data.Avro.Deriving
import Text.RawString.QQ

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "Record",
  "fields": [
    { "name": "name", "type": ["null", "string"], "default": null }
  ]
}
|]
