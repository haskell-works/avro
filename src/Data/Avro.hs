{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | Avro encoding and decoding routines.
--
-- This library provides a high level interface for encoding (and decoding)
-- Haskell values in Apache's Avro serialization format.
--
-- The goal is to match Aeson's API whenever reasonable,
-- meaning user experience with one effectively translate to the other.
--
-- Avro RPC is not currently supported.
--
-- == Library Structure
--
-- The library structure includes:
--
--   * This module, "Data.Avro", providing a high-level interface via
--     classes of 'FromAvro' and 'ToAvro' for decoding and encoding values.
--
--   * "Data.Avro.Schema": Defines the type for Avro schema's and its JSON
--      encoding/decoding.
--
--   * "Data.Avro.Encode" and "Data.Avro.Decode": More
--     efficient conversion capable of avoiding the intermediate representation.
--     Also, the implementation of the en/decoding of the intermediate
--     representation.
--
--   * "Data.Avro.Decode.Lazy": Lazy/Streaming decoding for Avro containers.
--
--   * "Data.Avro.Deconflict": translate decoded data from an
--     encoder schema to the (potentially different) decoder's schema.
module Data.Avro
  ( -- * Schema
    Schema(..)
  , Schema.Field(..), Schema.Order(..)
  , Schema.TypeName(..)
  , Schema.Decimal(..)
  , Schema.LogicalTypeBytes(..), Schema.LogicalTypeFixed(..)
  , Schema.LogicalTypeInt(..), Schema.LogicalTypeLong(..)
  , Schema.LogicalTypeString(..)

  -- * Deconflicting schemas
  , ReadSchema
  , deconflict
  , readSchemaFromSchema

  -- * Individual values
  , encodeValue
  , decodeValueWithSchema

  -- * Working with containers
  -- ** Decoding containers
  , decodeContainerWithEmbeddedSchema
  , decodeContainerWithReaderSchema
  , encodeContainer
  , encodeContainerWithSync
  , Container.newSyncBytes

  -- ** Extracting containers' data
  , extractContainerValuesBytes
  , decodeContainerValuesBytes

  -- * Classes
  , EncodeAvro
  , DecodeAvro

  -- * Compression
  , Codec, nullCodec, deflateCodec

  , HasAvroSchema(..)
  , schemaOf

  ) where

import           Control.Monad                 ((>=>))
import           Data.Avro.Codec               (Codec, deflateCodec, nullCodec)
import           Data.Avro.Encoding.DecodeAvro
import           Data.Avro.Encoding.EncodeAvro
import           Data.Avro.HasAvroSchema
import qualified Data.Avro.Internal.Container  as Container
import           Data.Avro.Schema.Deconflict   (deconflict)
import           Data.Avro.Schema.ReadSchema   (ReadSchema, fromSchema)
import           Data.Avro.Schema.Schema       (Schema)
import qualified Data.Avro.Schema.Schema       as Schema
import           Data.Binary.Get               (runGetOrFail)
import           Data.ByteString.Builder       (toLazyByteString)
import qualified Data.ByteString.Lazy          as BL

-- | Converts 'Schema' into 'ReadSchema'. This function may be useful when it is known
-- that the writer and the reader schemas are the same.
readSchemaFromSchema :: Schema -> ReadSchema
readSchemaFromSchema = fromSchema
{-# INLINE readSchemaFromSchema #-}

-- | Serialises an individual value into Avro.
encodeValue :: EncodeAvro a => Schema -> a -> BL.ByteString
encodeValue s = toLazyByteString . toEncoding s
{-# INLINE encodeValue #-}

-- | Deserialises an individual value from Avro.
decodeValueWithSchema :: DecodeAvro a => ReadSchema -> BL.ByteString -> Either String a
decodeValueWithSchema schema payload =
  case runGetOrFail (getValue schema) payload of
    Right (bs, _, v) -> fromValue v
    Left (_, _, e)   -> Left e

-- | Decodes the container as a lazy list of values of the requested type.
--
-- Errors are reported as a part of the list and the list will stop at first
-- error. This means that the consumer will get all the "good" content from
-- the container until the error is detected, then this error and then the list
-- is finished.
decodeContainerWithEmbeddedSchema :: forall a. DecodeAvro a => BL.ByteString -> [Either String a]
decodeContainerWithEmbeddedSchema payload =
  case Container.extractContainerValues (pure . fromSchema) (getValue >=> (either fail pure . fromValue)) payload of
    Left err          -> [Left err]
    Right (_, values) -> values

-- | Decodes the container as a lazy list of values of the requested type.
--
-- The provided reader schema will be de-conflicted with the schema
-- embedded with the container.
--
-- Errors are reported as a part of the list and the list will stop at first
-- error. This means that the consumer will get all the "good" content from
-- the container until the error is detected, then this error and then the list
-- is finished.
decodeContainerWithReaderSchema :: forall a. DecodeAvro a => Schema -> BL.ByteString -> [Either String a]
decodeContainerWithReaderSchema readerSchema payload =
  case Container.extractContainerValues (flip deconflict readerSchema) (getValue >=> (either fail pure . fromValue)) payload of
    Left err          -> [Left err]
    Right (_, values) -> values

-- | Splits container into a list of individual avro-encoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
extractContainerValuesBytes :: BL.ByteString -> Either String (Schema, [Either String BL.ByteString])
extractContainerValuesBytes =
  (fmap . fmap . fmap . fmap) snd . Container.extractContainerValuesBytes (pure . fromSchema) getValue
{-# INLINE extractContainerValuesBytes #-}

-- | Splits container into a list of individual avro-encoded values.
-- This version provides both encoded and decoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
decodeContainerValuesBytes :: forall a. DecodeAvro a
  => Schema
  -> BL.ByteString
  -> Either String (Schema, [Either String (a, BL.ByteString)])
decodeContainerValuesBytes readerSchema =
  Container.extractContainerValuesBytes (flip deconflict readerSchema) (getValue >=> (either fail pure . fromValue))
{-# INLINE decodeContainerValuesBytes #-}


-- |Encode chunks of objects into a container, using 16 random bytes for
-- the synchronization markers. Blocks are compressed (or not) according
-- to the given `Codec` (`nullCodec` or `deflateCodec`).
encodeContainer :: EncodeAvro a => Codec -> Schema -> [[a]] -> IO BL.ByteString
encodeContainer codec sch xss =
  do sync <- Container.newSyncBytes
     return $ encodeContainerWithSync codec sch sync xss

-- |Encode chunks of objects into a container, using the provided
-- ByteString as the synchronization markers.
encodeContainerWithSync :: EncodeAvro a => Codec -> Schema -> BL.ByteString -> [[a]] -> BL.ByteString
encodeContainerWithSync = Container.packContainerValuesWithSync' toEncoding
{-# INLINE encodeContainerWithSync #-}
