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
module Avro.Data.Endpoint
where

import           Data.Avro.Deriving (deriveAvroFromByteString)
import           Data.Text          (Text (..))
import qualified Data.Text          as Text
import           Text.RawString.QQ

import           Hedgehog       (Gen, MonadGen)
import qualified Hedgehog.Gen   as Gen
import           Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range


deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "Endpoint",
  "fields": [
    {
      "name": "opaque",
      "type": { "name": "Opaque", "type": "fixed", "size": 16 }
    },
    { "name": "correlation", "type": "Opaque" },
    { "name": "tag", "type": ["int", {"type": "string"}] },
    {
      "name": "ips",
      "type": { "type": "array", "items": "string" }
    },
    {
      "name": "ports",
      "type": {
        "type": "array",
        "items": {
          "type": "record",
          "name": "PortRange",
          "fields": [
            { "name": "start", "type": "int" },
            { "name": "end", "type": "int" }
          ]
        }
      }
    }
  ]
}
|]

ipGen :: MonadGen m => m Text
ipGen = do
  parts <- Gen.list (Range.singleton 4) (Gen.word Range.linearBounded)
  pure $ Text.intercalate "." (Text.pack . show <$> parts)


endpointGen :: MonadGen m => m Endpoint
endpointGen = do
  opq <- opaqueGen
  cor <- opaqueGen
  tag <- Gen.choice [Left <$> Gen.int32 Range.linearBounded, Right <$> Gen.text (Range.linear 0 64) Gen.alphaNum]
  ips <- Gen.list (Range.linear 0 20) ipGen
  pts <- Gen.list (Range.linear 0 8) portRangeGen
  pure $ Endpoint opq cor tag ips pts

portRangeGen :: MonadGen m => m PortRange
portRangeGen = do
  s <- Gen.int32 (Range.linear 0 32486)
  e <- Gen.int32 (Range.linear s maxBound)
  pure $ PortRange s e

opaqueGen :: MonadGen m => m Opaque
opaqueGen = Opaque <$> Gen.bytes (Range.singleton 16)
