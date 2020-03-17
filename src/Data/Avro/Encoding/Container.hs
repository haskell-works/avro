{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Avro.Encoding.Container
where

import           Control.Monad                 ((>=>))
import qualified Data.Aeson                    as Aeson
import           Data.Avro.Codec               (Codec (..))
import           Data.Avro.EncodeRaw           (encodeRaw)
import           Data.Avro.Encoding.DecodeAvro (getValue)
import           Data.Avro.Encoding.EncodeAvro (EncodeAvro, toEncoding)
import           Data.Avro.Encoding.Value      (DecodeAvro (..))
import           Data.Avro.Schema              (Schema)
import qualified Data.Avro.Schema              as Schema
import qualified Data.Avro.Schema.Deconflict   as Schema
import           Data.Avro.Schema.ReadSchema   (ReadSchema)
import qualified Data.Avro.Schema.ReadSchema   as ReadSchema
import           Data.Binary.Get               (Get)
import qualified Data.Binary.Get               as Get
import           Data.ByteString.Builder       (Builder, lazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy          as BL
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                      (Int32)
import           Data.Text                     (Text)

import Data.Avro.Internal.Container as Container

-- | Decodes the container as a lazy list of values of the requested type.
--
-- Errors are reported as a part of the list and the list will stop at first
-- error. This means that the consumer will get all the "good" content from
-- the container until the error is detected, then this error and then the list
-- is finished.
decodeContainerWithEmbeddedSchema :: forall a. DecodeAvro a => BL.ByteString -> [Either String a]
decodeContainerWithEmbeddedSchema payload =
  case extractContainerValues (pure . ReadSchema.fromSchema) (getValue >=> (either fail pure . fromValue)) payload of
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
  case Container.extractContainerValues (flip Schema.deconflict' readerSchema) (getValue >=> (either fail pure . fromValue)) payload of
    Left err          -> [Left err]
    Right (_, values) -> values

-- | Splits container into a list of individual avro-encoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
getContainerValuesBytes :: BL.ByteString -> Either String (Schema, [Either String BL.ByteString])
getContainerValuesBytes =
  (fmap . fmap . fmap . fmap) snd . Container.extractContainerValuesBytes (pure . ReadSchema.fromSchema) getValue
{-# INLINE getContainerValuesBytes #-}

-- | Splits container into a list of individual avro-encoded values.
-- This version provides both encoded and decoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
getContainerValuesBytes' :: forall a. DecodeAvro a
  => Schema
  -> BL.ByteString
  -> Either String (Schema, [Either String (a, BL.ByteString)])
getContainerValuesBytes' readerSchema =
  Container.extractContainerValuesBytes (flip Schema.deconflict' readerSchema) (getValue >=> (either fail pure . fromValue))
{-# INLINE getContainerValuesBytes' #-}


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
encodeContainerWithSync codec sch syncBytes xss =
 toLazyByteString $ containerHeaderWithSync codec sch syncBytes <> foldMap putBlocks xss
 where
  putBlocks ys =
    let nrObj    = length ys
        nrBytes  = BL.length theBytes
        theBytes = codecCompress codec $ toLazyByteString $ foldMap (toEncoding sch) ys
    in encodeRaw @Int32 (fromIntegral nrObj) <>
       encodeRaw nrBytes <>
       lazyByteString theBytes <>
       lazyByteString syncBytes

-- | Creates an Avro container header for a given schema.
containerHeaderWithSync :: Codec -> Schema -> BL.ByteString -> Builder
containerHeaderWithSync codec sch syncBytes =
  lazyByteString avroMagicBytes
    <> toEncoding (Schema.Map Schema.Bytes') headers
    <> lazyByteString syncBytes
  where
    avroMagicBytes :: BL.ByteString
    avroMagicBytes = "Obj" <> BL.pack [1]

    headers :: HashMap Text BL.ByteString
    headers =
      HashMap.fromList
        [
          ("avro.schema", Aeson.encode sch)
        , ("avro.codec", BL.fromStrict (codecName codec))
        ]
