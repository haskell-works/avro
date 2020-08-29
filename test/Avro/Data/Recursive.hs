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

module Avro.Data.Recursive
where

import Data.Avro.Internal.Time (microsToDiffTime, microsToUTCTime, millisToDiffTime, millisToUTCTime)

import Data.Avro.Deriving (deriveAvroFromByteString, r)

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "Recursive",
  "fields": [
    { "name": "index", "type": "int" },
    { "name": "tag", "type": ["null", "Recursive"] }
  ]
}
|]

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "RecursiveA",
  "fields": [
    { "name": "index", "type": "int" },
    { "name": "recursiveB",
      "type": {
        "type": "record",
        "name": "RecursiveB",
        "fields": [
          { "name": "index", "type": "int"},
          { "name": "recursiveA", "type": ["null", "RecursiveA"] }
        ]
      }
    }
  ]
}
|]


recursiveGen :: MonadGen m => m Recursive
recursiveGen = Recursive
  <$> Gen.int32 Range.linearBounded
  <*> Gen.maybe recursiveGen

recursiveAGen :: MonadGen m => m RecursiveA
recursiveAGen = RecursiveA
  <$> Gen.int32 Range.linearBounded
  <*> recursiveBGen

recursiveBGen :: MonadGen m => m RecursiveB
recursiveBGen = RecursiveB
  <$> Gen.int32 Range.linearBounded
  <*> Gen.maybe recursiveAGen

-- maybeTestGen :: MonadGen m => m MaybeTest
-- maybeTestGen = MaybeTest
--   <$> Gen.maybe (Gen.text (Range.linear 0 50) Gen.alphaNum)
--   <*> (FixedTag <$> Gen.bytes (Range.singleton 3))
--   <*> Gen.bytes (Range.linear 0 30)
