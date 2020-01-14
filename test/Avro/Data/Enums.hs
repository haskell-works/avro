{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Avro.Data.Enums
where

import Data.Avro.Deriving (deriveAvroFromByteString)
import Text.RawString.QQ

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "EnumWrapper",
  "namespace": "haskell.avro.example",
  "fields": [
    { "name": "id",     "type": "long" },
    { "name": "name",   "type": "string"},
    { "name": "reason",
    "type": {
        "type": "enum",
        "name": "EnumReason",
        "symbols": ["Because", "Instead"]
      }
    }
  ]
}
|]

enumWrapperGen :: MonadGen m => m EnumWrapper
enumWrapperGen = do
  lng <- Gen.int64 Range.linearBounded
  txt <- Gen.text (Range.linear 0 1024) Gen.alphaNum
  rsn <- Gen.enumBounded
  pure $ EnumWrapper lng txt rsn

-- enumReasonGen :: MonadGen m => m EnumReason
-- enumReasonGen = en
