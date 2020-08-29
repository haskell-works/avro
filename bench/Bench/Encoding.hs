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

module Bench.Encoding
where

import           Control.DeepSeq
import           Data.Avro               (decodeContainerWithEmbeddedSchema, encodeContainer, encodeContainerWithSchema, encodeValueWithSchema, nullCodec)
import qualified Data.Avro               as Avro
import           Data.Avro.Deriving      (deriveAvroFromByteString, r)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as BL
import           Data.List               (unfoldr)
import qualified Data.Vector             as Vector
import qualified System.Random           as Random

import Gauge

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "Outer",
  "fields": [
    { "name": "name", "type": "string" },
    { "name": "inner", "type": {
        "type": "record",
        "name": "Inner",
        "fields": [
          { "name": "id", "type": "int" }
        ]
      }
    },
    { "name": "other", "type": "Inner" }
  ]
}
|]

deriving instance NFData Inner
deriving instance NFData Outer

newOuter :: IO Outer
newOuter = do
  i1 <- Random.randomRIO (minBound, maxBound)
  i2 <- Random.randomRIO (minBound, maxBound)
  pure $ Outer "Written" (Inner i1) (Inner i2)

many :: Int -> IO a -> IO (Vector.Vector a)
many = Vector.replicateM

encodeToBS :: Benchmark
encodeToBS = env (many 1e5 newOuter) $ \ values ->
  bgroup "Encode to ByteString"
    [ bgroup "Simple type"
        [ bench "Encode via ToAvro" $ nf (fmap (BL.toStrict . encodeValueWithSchema schema'Outer)) values
        ]
    ]

encodeContainer :: Benchmark
encodeContainer = env (chunksOf 100 . Vector.toList <$> many 1e5 newOuter) $ \values ->
  bgroup "Encode container"
    [ bench "Via ToAvro" $ nfIO $ Avro.encodeContainerWithSchema nullCodec schema'Outer values
    ]

roundtripContainer :: Benchmark
roundtripContainer = env (chunksOf 100 . Vector.toList <$> many 1e5 newOuter) $ \values ->
  bgroup "Roundtrip container"
    [ bench "Via ToAvro/FromAvro" $ nfIO $ decodeContainerWithEmbeddedSchema @Outer <$> Avro.encodeContainerWithSchema nullCodec schema'Outer values
    , bench "Via ToAvro/FromAvro/HasAvroSchema" $ nfIO $ decodeContainerWithEmbeddedSchema @Outer <$> Avro.encodeContainer nullCodec values
    ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not.null) . unfoldr (Just . splitAt n)
