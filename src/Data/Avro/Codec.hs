{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Avro.Codec (
    Codec(..)
  , nullCodec
  , deflateCodec
  ) where

import           Codec.Compression.Zlib.Internal as Zlib
import           Control.Monad.ST.Lazy
import qualified Data.Binary.Get                 as G
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS

-- | A `Codec` allows for compression/decompression of a block in an
-- Avro container according to the Avro spec.
data Codec = Codec
  {
    -- | The name of the codec according to the Avro spec.
    codecName       :: ByteString
    -- | Execute a `G.Get` over a chunk of bytes possibly decompressing
    -- the chunk incrementally.
    --
    -- The API is somewhat more complex than say `codecCompress` to allow
    -- interleaving of decompression and binary decoding.
  , codecDecompress :: forall a. LBS.ByteString -> G.Get a -> G.Get a

    -- | Compresses a lazy stream of bytes.
  , codecCompress   :: LBS.ByteString -> LBS.ByteString
  }

-- | `nullCodec` specifies @null@ required by Avro spec.
-- (see https://avro.apache.org/docs/1.8.1/spec.html#null)
nullCodec :: Codec
nullCodec =
  Codec
    {
      codecName = "null"
    , codecDecompress = \input parser ->
        case G.runGetOrFail parser input of
          Right (_, _, x)  -> pure x
          Left (_, _, err) -> fail err
    , codecCompress   = id
    }

-- | `deflateCodec` specifies @deflate@ codec required by Avro spec.
-- (see https://avro.apache.org/docs/1.8.1/spec.html#deflate)
deflateCodec :: Codec
deflateCodec =
  Codec
    {
      codecName       = "deflate"
    , codecDecompress = deflateDecompress
    , codecCompress   = deflateCompress
    }

deflateCompress :: LBS.ByteString -> LBS.ByteString
deflateCompress =
  Zlib.compress Zlib.rawFormat Zlib.defaultCompressParams


-- | Internal type to help constructy a lazy list of
-- decompressed bytes interleaved with errors if any.
data Chunk
  = ChunkRest LBS.ByteString
  | ChunkBytes ByteString
  | ChunkError Zlib.DecompressError


deflateDecompress :: forall a. LBS.ByteString -> G.Get a -> G.Get a
deflateDecompress bytes parser = do
  let
    -- N.B. this list is lazily created thus allowing us to
    -- interleave decompression and binary decoding.
    chunks :: [Chunk]
    chunks =
      Zlib.foldDecompressStreamWithInput
        (\x xs -> ChunkBytes x : xs)
        (\rest -> [ChunkRest rest])
        (\err -> [ChunkError err])
        (Zlib.decompressST Zlib.rawFormat Zlib.defaultDecompressParams)
        bytes

    decode :: G.Decoder a -> [Chunk] -> G.Get (G.Decoder a)
    decode !dec@G.Fail{} _ =
      -- short circuit if decoding failed
      pure dec
    decode !dec [] =
      pure dec
    decode !dec (inchunk : inchunks) =
      case inchunk of
        ChunkBytes x ->
          decode (G.pushChunk dec x) inchunks
        ChunkError err ->
          fail (show err)
        ChunkRest rest -> do
          let
            dec' = G.pushEndOfInput dec
          pure $ G.pushChunks dec' rest

  dec <- decode (G.runGetIncremental parser) chunks

  case dec of
    G.Fail _ _ err ->
      fail err
    G.Partial{} ->
      fail "Not enough input"
    G.Done _ _ x ->
      pure x
