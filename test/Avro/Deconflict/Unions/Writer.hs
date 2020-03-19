{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Avro.Deconflict.Unions.Writer where

import Data.Avro.Deriving

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "Record",
  "fields": [
    { "name": "name", "type": "string" }
  ]
}
|]

recordGen :: MonadGen m => m Record
recordGen = do
  nam <- Gen.text (Range.linear 0 512) Gen.ascii -- unicodeAll
  pure $ Record nam
