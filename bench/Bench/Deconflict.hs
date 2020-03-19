{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Bench.Deconflict
( notOnly
, container
)
where

import Data.Avro                   (decodeContainerWithReaderSchema, decodeValueWithSchema, encodeContainer, encodeValue, nullCodec)
import Data.Avro.Schema.ReadSchema (fromSchema)
import Data.Vector                 (Vector)

import qualified Bench.Deconflict.Reader as R
import qualified Bench.Deconflict.Writer as W
import qualified Data.Vector             as Vector
import qualified System.Random           as Random

import Gauge

newOuter :: IO W.Outer
newOuter = do
  i1 <- Random.randomRIO (minBound, maxBound)
  i2 <- Random.randomRIO (minBound, maxBound)
  pure $ W.Outer "Written" (W.Inner i1) (W.Inner i2)

many :: Int -> IO a -> IO (Vector a)
many = Vector.replicateM

notOnly :: Benchmark
notOnly = env (many 1e5 $ encodeValue W.schema'Outer <$> newOuter) $ \ values ->
  let
    readSchema = fromSchema W.schema'Outer
  in bgroup "Encoded: ByteString"
      [ bgroup "No Deconflict"
          [ bench "Read via Encoding"      $ nf (fmap (decodeValueWithSchema @W.Outer readSchema)) values
          ]
      ]

container :: Benchmark
container = env (many 1e5 newOuter >>= (\vs -> encodeContainer nullCodec W.schema'Outer [Vector.toList vs])) $ \payload ->
  bgroup "Decoding container"
    [ bench "From Encoding" $ nf (\v -> decodeContainerWithReaderSchema @R.Outer R.schema'Outer v) payload
    ]
