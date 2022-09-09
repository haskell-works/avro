{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
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
{-# options -fprint-potential-instances #-}

module Avro.Data.Deconflict.Write where

import Data.Avro.Deriving
import Data.Avro.Sum

import           Hedgehog       (Gen, MonadGen)
import qualified Hedgehog.Gen   as Gen
import           Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range

deriveAvroFromByteString [r|
[
{ "name": "Foo",
  "type": "record",
  "fields": [
    { "name": "fooBar",
      "type": {
        "name": "Bar",
        "type": "record",
        "fields": [
          { "name": "barInt",     "type": "int", "order": "ascending" },
          { "name": "barTime",    "type": "int" },
          { "name": "barLong",    "type": "long" },
          { "name": "barString",  "type": "string" },
          { "name": "barUnion",   "type": ["string", "long"], "default": "Hello"}
        ]
      }
    },
    { "name": "fooOption", "type": ["null", "string"], "default": null },
    { "name": "fooUnion",  "type": ["int", "string", "float"] }
  ]
}
]
|]

genBar :: MonadGen m => m Bar
genBar = Bar
  <$> Gen.int32 Range.linearBounded
  <*> Gen.int32 Range.linearBounded
  <*> Gen.int64 Range.linearBounded
  <*> Gen.text (Range.linear 0 256) Gen.unicode
  <*> Gen.choice
        [ makeNSum <$> Gen.int64 Range.linearBounded
        , makeNSum <$> Gen.text (Range.linear 0 256) Gen.unicode
        ]

genFoo :: MonadGen m => m Foo
genFoo = Foo
  <$> genBar
  <*> Gen.maybe (Gen.text (Range.linear 0 256) Gen.unicode)
  <*> Gen.choice
        [ makeNSum <$> Gen.int32  Range.linearBounded
        , makeNSum <$> Gen.text  (Range.linear 0 256) Gen.unicode
        , makeNSum <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        ]
