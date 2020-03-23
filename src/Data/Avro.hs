{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | Avro encoding and decoding routines.
--
-- This library provides a high level interface for encoding and decoding
-- Haskell values in Apache's Avro serialization format.
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
  , encodeValueWithSchema

  , decodeValue
  , decodeValueWithSchema

  -- * Working with containers
  -- ** Decoding containers
  , decodeContainerWithEmbeddedSchema
  , decodeContainerWithReaderSchema

  , encodeContainer
  , encodeContainerWithSchema
  , encodeContainerWithSync
  , Container.newSyncBytes

  -- ** Extracting containers' data
  , extractContainerValuesBytes
  , decodeContainerValuesBytes

  -- * Classes
  , ToAvro
  , FromAvro

  -- * Compression
  , Codec, nullCodec, deflateCodec

  , HasAvroSchema(..)
  , schemaOf

  ) where

import           Control.Monad                ((>=>))
import           Data.Avro.Codec              (Codec, deflateCodec, nullCodec)
import           Data.Avro.Encoding.FromAvro
import           Data.Avro.Encoding.ToAvro
import           Data.Avro.HasAvroSchema
import qualified Data.Avro.Internal.Container as Container
import           Data.Avro.Schema.Deconflict  (deconflict)
import           Data.Avro.Schema.ReadSchema  (ReadSchema, fromSchema)
import           Data.Avro.Schema.Schema      (Schema)
import qualified Data.Avro.Schema.Schema      as Schema
import           Data.Binary.Get              (runGetOrFail)
import           Data.ByteString.Builder      (toLazyByteString)
import qualified Data.ByteString.Lazy         as BL
import           Data.Tagged                  (untag)

-- | Converts 'Schema' into 'ReadSchema'. This function may be useful when it is known
-- that the writer and the reader schemas are the same.
readSchemaFromSchema :: Schema -> ReadSchema
readSchemaFromSchema = fromSchema
{-# INLINE readSchemaFromSchema #-}

-- | Serialises an individual value into Avro with the schema provided.
encodeValueWithSchema :: ToAvro a => Schema -> a -> BL.ByteString
encodeValueWithSchema s = toLazyByteString . toAvro s
{-# INLINE encodeValueWithSchema #-}

-- | Serialises an individual value into Avro using the schema
-- from its coresponding 'HasAvroSchema' instance.
encodeValue :: (HasAvroSchema a, ToAvro a) => a -> BL.ByteString
encodeValue a = encodeValueWithSchema (schemaOf a) a
{-# INLINE encodeValue #-}

-- | Deserialises an individual value from Avro.
decodeValueWithSchema :: FromAvro a => ReadSchema -> BL.ByteString -> Either String a
decodeValueWithSchema schema payload =
  case runGetOrFail (getValue schema) payload of
    Right (bs, _, v) -> fromAvro v
    Left (_, _, e)   -> Left e

-- | Deserialises an individual value from Avro using the schema from its coresponding 'HasAvroSchema'.
--
-- __NOTE__: __This function is only to be used when reader and writes schemas are known to be the same.__
-- Because only one schema is known at this point, and it is the reader schema,
-- /no decondlicting/ can be performed.
decodeValue :: forall a. (HasAvroSchema a, FromAvro a) => BL.ByteString -> Either String a
decodeValue = decodeValueWithSchema (fromSchema (untag @a schema))

-- | Decodes the container as a lazy list of values of the requested type.
--
-- Errors are reported as a part of the list and the list will stop at first
-- error. This means that the consumer will get all the "good" content from
-- the container until the error is detected, then this error and then the list
-- is finished.
decodeContainerWithEmbeddedSchema :: forall a. FromAvro a => BL.ByteString -> [Either String a]
decodeContainerWithEmbeddedSchema payload =
  case Container.extractContainerValues (pure . fromSchema) (getValue >=> (either fail pure . fromAvro)) payload of
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
decodeContainerWithReaderSchema :: forall a. FromAvro a => Schema -> BL.ByteString -> [Either String a]
decodeContainerWithReaderSchema readerSchema payload =
  case Container.extractContainerValues (flip deconflict readerSchema) (getValue >=> (either fail pure . fromAvro)) payload of
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
decodeContainerValuesBytes :: forall a. FromAvro a
  => Schema
  -> BL.ByteString
  -> Either String (Schema, [Either String (a, BL.ByteString)])
decodeContainerValuesBytes readerSchema =
  Container.extractContainerValuesBytes (flip deconflict readerSchema) (getValue >=> (either fail pure . fromAvro))
{-# INLINE decodeContainerValuesBytes #-}

-- | Encode chunks of values into a container, using 16 random bytes for
-- the synchronization markers and a corresponding 'HasAvroSchema' schema.
-- Blocks are compressed (or not) according to the given 'Codec' ('nullCodec' or 'deflateCodec').
encodeContainer :: forall a. (HasAvroSchema a, ToAvro a) => Codec -> [[a]] -> IO BL.ByteString
encodeContainer codec = encodeContainerWithSchema codec (untag @a schema)

-- | Encode chunks of values into a container, using 16 random bytes for
-- the synchronization markers. Blocks are compressed (or not) according
-- to the given 'Codec' ('nullCodec' or 'deflateCodec').
encodeContainerWithSchema :: ToAvro a => Codec -> Schema -> [[a]] -> IO BL.ByteString
encodeContainerWithSchema codec sch xss =
  do sync <- Container.newSyncBytes
     return $ encodeContainerWithSync codec sch sync xss

-- |Encode chunks of objects into a container, using the provided
-- ByteString as the synchronization markers.
encodeContainerWithSync :: ToAvro a => Codec -> Schema -> BL.ByteString -> [[a]] -> BL.ByteString
encodeContainerWithSync = Container.packContainerValuesWithSync' toAvro
{-# INLINE encodeContainerWithSync #-}
