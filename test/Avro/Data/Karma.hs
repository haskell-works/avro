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
module Avro.Data.Karma
where

import Data.Avro.Deriving (deriveAvroFromByteString)
import Text.RawString.QQ

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

deriveAvroFromByteString [r|
[{
    "type": "record",
    "name": "Blessing",
    "namespace": "avro.test.data",
    "fields": [
      { "name": "geo",
        "type": ["null", {
          "type": "record",
          "name": "Geo",
          "fields": [
            { "name": "source", "type": "string" },
            { "name": "dest", "type": "string" }
          ]
        }]
      }
    ]
  },
  {
    "type": "record",
    "name": "Curse",
    "namespace": "avro.test.data",
    "fields": [
      { "name": "geo", "type": ["null", "Geo"] }
    ]
  }
]
|]

geoGen :: MonadGen m => m Geo
geoGen = do
  src <- Gen.element ["AU", "CN", "DE", "KZ", "LU"]
  dst <- Gen.element ["BE", "LX", "NZ", "NL", "UK"]
  pure $ Geo src dst

blessingGen :: MonadGen m => m Blessing
blessingGen = Blessing <$> Gen.maybe geoGen

curseGen :: MonadGen m => m Curse
curseGen = Curse <$> Gen.maybe geoGen
