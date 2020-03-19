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
module Avro.Data.FixedTypes
where

import Data.Avro.Deriving (deriveAvroFromByteString, r)

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

deriveAvroFromByteString [r|
{
  "name": "ReuseFixed",
  "type": "record",
  "fields": [
    {
      "name": "primary",
      "type": {
        "type": "fixed",
        "name": "FixedData",
        "size": 16
      }
    },
    {
      "name": "secondary",
      "type": "FixedData"
    }
  ]
}
|]

fixedDataGen :: MonadGen m => m FixedData
fixedDataGen = FixedData <$> Gen.bytes (Range.singleton 16)

reuseFixedGen :: MonadGen m => m ReuseFixed
reuseFixedGen = ReuseFixed <$> fixedDataGen <*> fixedDataGen
