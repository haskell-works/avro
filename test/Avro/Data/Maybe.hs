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
module Avro.Data.Maybe
where

import Data.Avro.Internal.Time (microsToDiffTime, microsToUTCTime, millisToDiffTime, millisToUTCTime)

import Data.Avro.Deriving (deriveAvroFromByteString)
import Text.RawString.QQ

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "MaybeTest",
  "fields": [
    { "name": "tag", "type": ["null", "string"], "default": null },
    { "name": "fixedTag",
      "type": {
        "type": "fixed",
        "name": "FixedTag",
        "size": 3
      },
      "default": "\u0000\u002a\u00ff"
    },
    { "name": "bytesTag",
      "type": "bytes",
      "default": "\u0000\u0025\u00ff"
    }
  ]
}
|]

maybeTestGen :: MonadGen m => m MaybeTest
maybeTestGen = MaybeTest
  <$> Gen.maybe (Gen.text (Range.linear 0 50) Gen.alphaNum)
  <*> (FixedTag <$> Gen.bytes (Range.singleton 3))
  <*> Gen.bytes (Range.linear 0 30)
