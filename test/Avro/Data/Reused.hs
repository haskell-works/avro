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

module Avro.Data.Reused
where

import Data.Avro.Deriving (deriveAvroFromByteString, r)

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "ReusedWrapper",
  "namespace": "Boo",
  "fields": [
    {
      "name": "full",
      "type": {
        "type": "record",
        "name": "ReusedChild",
        "fields": [
          {
            "name": "data",
            "type": "int"
          }
        ]
      }
    },
    {
      "name": "inner",
      "type": {
        "type": "record",
        "name": "ContainerChild",
        "fields": [
          {
            "name": "fstIncluded",
            "type": "ReusedChild"
          },
          {
            "name": "sndIncluded",
            "type": "ReusedChild"
          }
        ]
      }
    }
  ]
}
|]

reusedWrapperGen :: MonadGen m => m ReusedWrapper
reusedWrapperGen = ReusedWrapper <$> reusedChildGen <*> containerChildGen

reusedChildGen :: MonadGen m => m ReusedChild
reusedChildGen = ReusedChild <$> Gen.int32 Range.linearBounded

containerChildGen :: MonadGen m => m ContainerChild
containerChildGen = ContainerChild <$> reusedChildGen <*> reusedChildGen
