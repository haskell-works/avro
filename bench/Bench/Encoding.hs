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
module Bench.Encoding
where

import           Control.DeepSeq
import           Data.Avro                     (encode)
import qualified Data.Avro                     as Avro
import           Data.Avro.Codec               (nullCodec)
import           Data.Avro.Deriving            (deriveAvroFromByteString)
import qualified Data.Avro.Encoding.Container  as Encoding
import           Data.Avro.Encoding.EncodeAvro (encodeAvro)
import           Data.ByteString               (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as BL
import           Data.List                     (unfoldr)
import qualified Data.Vector                   as Vector
import qualified System.Random                 as Random
import           Text.RawString.QQ

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
        [ bench "Encode via ToAvro"     $ nf (fmap (BL.toStrict . encode)) values
        , bench "Encode via EncodeAvro" $ nf (fmap (BL.toStrict . encodeAvro schema'Outer)) values
        ]
    -- , bgroup "deconflict"
    --     [ bench "plain"     $ nf (fmap (deconflict          W.schema'Outer R.schema'Outer)) $ values
    --     , bench "noResolve" $ nf (fmap (deconflictNoResolve W.schema'Outer R.schema'Outer)) $ values
    --     ]
    ]

encodeContainer :: Benchmark
encodeContainer = env (chunksOf 100 . Vector.toList <$> many 1e5 newOuter) $ \values ->
  bgroup "Encode container"
    [ bench "Via ToAvro" $ nfIO $ Avro.encodeContainer values
    , bench "Via EncodeAvro" $ nfIO $ Encoding.encodeContainer nullCodec schema'Outer values
    ]

roundtripContainer :: Benchmark
roundtripContainer = env (chunksOf 100 . Vector.toList <$> many 1e5 newOuter) $ \values ->
  bgroup "Roundtrip container"
    [ bench "Via ToAvro/FromAvro" $ nfIO $ Avro.decodeContainer @Outer <$> Avro.encodeContainer values
    , bench "Via EncodeAvro/DecodeAvro" $ nfIO $ Encoding.decodeContainerWithEmbeddedSchema @Outer <$> Encoding.encodeContainer nullCodec schema'Outer values
    ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not.null) . unfoldr (Just . splitAt n)
