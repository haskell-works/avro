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

module Avro.Data.Logical
where

import Data.Avro.Internal.Time (microsToDiffTime, microsToLocalTime, microsToUTCTime, millisToDiffTime, millisToLocalTime, millisToUTCTime)

import Data.Avro.Deriving (deriveAvroFromByteString, r)

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

deriveAvroFromByteString [r|
{
  "name": "Logical",
  "type": "record",
  "fields": [
    {
      "name": "tsMillis",
      "type":
        {
          "logicalType": "timestamp-millis",
          "type": "long"
        }
    },
    {
      "name": "tsMicros",
      "type":
        {
          "logicalType": "timestamp-micros",
          "type": "long"
        }
    },
    {
      "name": "timeMillis",
      "type":
        {
          "logicalType": "time-millis",
          "type": "int"
        }
    },
    {
      "name": "timeMicros",
      "type":
        {
          "logicalType": "time-micros",
          "type": "long"
        }
    },
    {
      "name": "localTimestampMillis",
      "type":
        {
          "logicalType": "local-timestamp-millis",
          "type": "long"
        }
     },
     {
      "name": "localTimestampMicros",
      "type":
        {
          "logicalType": "local-timestamp-micros",
          "type": "long"
        }
     }
  ]
}
|]

logicalGen :: MonadGen m => m Logical
logicalGen = Logical
  <$> (millisToUTCTime  . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
  <*> (microsToUTCTime  . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
  <*> (millisToDiffTime . toInteger <$> Gen.int32 (Range.linear 0 maxBound))
  <*> (microsToDiffTime . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
  <*> (millisToLocalTime . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
  <*> (microsToLocalTime . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
