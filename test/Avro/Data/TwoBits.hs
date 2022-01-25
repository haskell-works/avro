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

module Avro.Data.TwoBits
where

import Data.Avro.Deriving (deriveAvroFromByteString, r)

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

import qualified Data.ByteString.Lazy as LBS

twoBits'rawSchema :: LBS.ByteString
twoBits'rawSchema = [r|
{
  "type": "record",
  "name": "TwoBits",
  "fields": [
    { "name": "bit0",
      "type": {
        "type": "enum",
        "name": "Bit",
        "symbols": [ "Zero", "One"]
      }
    },
    { "name": "bit1",
      "type": "Bit"
    }
  ]
}
|]


deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "TwoBits",
  "fields": [
    { "name": "bit0",
      "type": {
        "type": "enum",
        "name": "Bit",
        "symbols": [ "Zero", "One"]
      }
    },
    { "name": "bit1",
      "type": "Bit"
    }
  ]
}
|]
