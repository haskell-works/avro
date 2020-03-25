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
module Avro.Data.Deconflict.Write where

import Data.Avro.Deriving
import Data.Avro.EitherN

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
          { "name": "barInt",     "type": "int" },
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
        [ Right <$> Gen.int64 Range.linearBounded
        , Left  <$> Gen.text (Range.linear 0 256) Gen.unicode
        ]

genFoo :: MonadGen m => m Foo
genFoo = Foo
  <$> genBar
  <*> Gen.maybe (Gen.text (Range.linear 0 256) Gen.unicode)
  <*> Gen.choice
        [ E3_1 <$> Gen.int32  Range.linearBounded
        , E3_2 <$> Gen.text  (Range.linear 0 256) Gen.unicode
        , E3_3 <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        ]
