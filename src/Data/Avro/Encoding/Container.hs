{-# LANGUAGE ScopedTypeVariables #-}
module Data.Avro.Encoding.Container
where

import           Control.Monad                   ((>=>))
import           Data.Avro.Encoding.FromEncoding (getValue)
import           Data.Avro.Encoding.Value        (FromValue (..))
import           Data.Avro.Schema                (Schema)
import qualified Data.Avro.Schema.Deconflict     as Schema
import           Data.Avro.Schema.ReadSchema     (ReadSchema)
import qualified Data.Avro.Schema.ReadSchema     as Schema
import           Data.Binary.Get                 (Get)
import qualified Data.Binary.Get                 as Get
import qualified Data.ByteString.Lazy            as BL

import Data.Avro.Internal.Container (consumeN, decodeRawBlocks, extractContainerValues, extractContainerValuesBytes)

-- | Decodes the container as a lazy list of values of the requested type.
--
-- Errors are reported as a part of the list and the list will stop at first
-- error. This means that the consumer will get all the "good" content from
-- the container until the error is detected, then this error and then the list
-- is finished.
decodeContainerWithEmbeddedSchema :: forall a. FromValue a => BL.ByteString -> [Either String a]
decodeContainerWithEmbeddedSchema payload =
  case extractContainerValues (pure . Schema.fromSchema) (getValue >=> (either fail pure . fromValue)) payload of
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
decodeContainerWithReaderSchema :: forall a. FromValue a => Schema -> BL.ByteString -> [Either String a]
decodeContainerWithReaderSchema readerSchema payload =
  case extractContainerValues (flip Schema.deconflict' readerSchema) (getValue >=> (either fail pure . fromValue)) payload of
    Left err          -> [Left err]
    Right (_, values) -> values

-- | Splits container into a list of individual avro-encoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
getContainerValuesBytes :: BL.ByteString -> Either String (Schema, [Either String BL.ByteString])
getContainerValuesBytes =
  (fmap . fmap . fmap . fmap) snd . extractContainerValuesBytes (pure . Schema.fromSchema) getValue
{-# INLINE getContainerValuesBytes #-}

-- | Splits container into a list of individual avro-encoded values.
-- This version provides both encoded and decoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
getContainerValuesBytes' :: forall a. FromValue a
  => Schema
  -> BL.ByteString
  -> Either String (Schema, [Either String (a, BL.ByteString)])
getContainerValuesBytes' readerSchema =
  extractContainerValuesBytes (flip Schema.deconflict' readerSchema) (getValue >=> (either fail pure . fromValue))
{-# INLINE getContainerValuesBytes' #-}
