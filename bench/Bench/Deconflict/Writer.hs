{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE DeriveAnyClass   #-}
module Bench.Deconflict.Writer
where

import Data.Avro.Deriving
import Text.RawString.QQ

import Control.DeepSeq

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
          { "name": "id", "type": "int" }
        ]
      }
    },
    { "name": "other", "type": "Inner" }
  ]
}
|]

deriving instance NFData Inner
deriving instance NFData Outer