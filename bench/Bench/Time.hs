{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Time where

import Control.Monad (replicateM)

import qualified Data.ByteString.Lazy as LBS
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector

import Gauge

import GHC.Int (Int32, Int64)

import qualified System.Random as Random

import qualified Data.Avro             as Avro
import qualified Data.Avro.Decode      as Decode
import qualified Data.Avro.Encode      as Encode
import           Data.Avro.Schema      (Schema)
import qualified Data.Avro.Schema      as Schema
import           Data.Avro.Types.Value (Value)
import qualified Data.Avro.Types.Value as Value
-- * Encoding to binary

encode :: Benchmark
encode = bgroup "encode" [ encodeArray ]

encodeArray :: Benchmark
encodeArray = env randoms $ \ ~(bools, ints, longs, records) ->
  bgroup "array"
  [ bench "bools" $
      nf encodeAvro $ Value.Array $ Value.Boolean <$> bools
  , bench "ints" $
      nf encodeAvro $ Value.Array $ (Value.Int Schema.Int') <$> ints
  , bench "longs" $
      nf encodeAvro $ Value.Array $ (Value.Long Schema.Long') <$> longs
  , bench "records" $
      nf encodeAvro $ Value.Array records
  ]
  where randoms = do
          bools <- array
          ints  <- array
          longs <- array
          pure (bools, ints, longs, records bools ints longs)

        array :: (Bounded r, Random.Random r) => IO (Vector r)
        array = Vector.replicateM 1e5 (Random.randomRIO (minBound, maxBound))

        records bools ints longs =
          Vector.zipWith3 record bools ints longs
        record bool int long = Value.Record recordSchema
          [ ("b", Value.Boolean bool)
          , ("i", (Value.Int Schema.Int') int)
          , ("l", (Value.Long Schema.Long') long)
          ]
        recordSchema = Schema.Record "Rec" [] Nothing Nothing
          [ Schema.Field "b" [] Nothing Nothing (Schema.AsIs 0) Schema.Boolean Nothing
          , Schema.Field "i" [] Nothing Nothing (Schema.AsIs 1) Schema.Int' Nothing
          , Schema.Field "l" [] Nothing Nothing (Schema.AsIs 2) Schema.Long' Nothing
          ]

encodeAvro :: Value Schema -> LBS.ByteString
encodeAvro = Encode.encodeAvro

-- * Decoding from binary

decode :: Benchmark
decode = bgroup "decode" [ decodeArray ]

decodeArray :: Benchmark
decodeArray = env randoms $ \ ~(bools, ints, longs, records) ->
  bgroup "array"
  [ bench "bools" $
      nf (decodeAvro $ Schema.Array Schema.Boolean) bools
  , bench "ints" $
      nf (decodeAvro $ Schema.Array Schema.Int') ints
  , bench "longs" $
      nf (decodeAvro $ Schema.Array Schema.Long') longs
  , bench "records" $
      nf (decodeAvro $ Schema.Array recordSchema) records
  ]
  where randoms = do
          bools <- array
          ints  <- array
          longs <- array
          pure ( encodeAvro $ Value.Array $ Value.Boolean <$> bools
               , encodeAvro $ Value.Array $ Value.Int Schema.Int' <$> ints
               , encodeAvro $ Value.Array $ Value.Long Schema.Long' <$> longs
               , encodeAvro $ Value.Array $ records bools ints longs
               )

        array :: (Bounded r, Random.Random r) => IO (Vector r)
        array = Vector.replicateM 1e5 (Random.randomRIO (minBound, maxBound))

        records bools ints longs =
          Vector.zipWith3 record bools ints longs
        record bool int long = Value.Record recordSchema
          [ ("b", Value.Boolean bool)
          , ("i", Value.Int Schema.Int' int)
          , ("l", Value.Long Schema.Long' long)
          ]
        recordSchema = Schema.Record "Rec" [] Nothing Nothing
          [ Schema.Field "b" [] Nothing Nothing (Schema.AsIs 0) Schema.Boolean Nothing
          , Schema.Field "i" [] Nothing Nothing (Schema.AsIs 1) Schema.Int' Nothing
          , Schema.Field "l" [] Nothing Nothing (Schema.AsIs 2) Schema.Long' Nothing
          ]

decodeAvro :: Schema -> LBS.ByteString -> Value Schema
decodeAvro schema bytes = case Decode.decodeAvro schema bytes of
  Left err  -> error err
  Right res -> res
