{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    Schema

    -- * Encoding and decoding
  , Result(..), badValue
  , encode
  , decode

  , (.:)
  , (.=), record, fixed

    -- * Working with containers
    -- ** Decoding containers
  , decodeWithSchema
  , decodeContainer
  , decodeContainerWithSchema
  , decodeContainerBytes

    -- ** Encoding containers
  , encodeContainer
  , encodeContainer'
  , encodeContainerWithSync
  , encodeContainerWithSync'

  -- * Classes and instances
  , FromAvro(..)
  , ToAvro(..)
  , HasAvroSchema(..)
  , schemaOf

  -- * Misc
  , Avro
  ) where

import           Control.Arrow         (first)
import qualified Data.Avro.Decode      as D
import qualified Data.Avro.Decode.Lazy as DL
import           Data.Avro.Deconflict  as C
import qualified Data.Avro.Encode      as E
import           Data.Avro.Schema      as S
import           Data.Avro.Types       as T
import qualified Data.Binary.Get       as G
import qualified Data.Binary.Put       as P
import qualified Data.ByteString       as B
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as BL
import           Data.Foldable         (toList)
import qualified Data.HashMap.Strict   as HashMap
import           Data.Int
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.Map              as Map
import           Data.Monoid           ((<>))
import           Data.Tagged
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Lazy        as TL
import qualified Data.Vector           as V
import           Data.Word
import           Prelude               as P

import Data.Avro.Codec         (Codec, deflateCodec, nullCodec)
import Data.Avro.FromAvro
import Data.Avro.HasAvroSchema
import Data.Avro.ToAvro

type Avro a = (FromAvro a, ToAvro a)

-- | Decode a lazy bytestring using a 'Schema' of the return type.
decode :: forall a. FromAvro a => ByteString -> Result a
decode bytes =
  case D.decodeAvro (untag (schema :: Tagged a Schema)) bytes of
      Right val -> fromAvro val
      Left err  -> Error err

-- | Decode a lazy bytestring using a provided schema
decodeWithSchema :: FromAvro a => Schema -> ByteString -> Result a
decodeWithSchema sch bytes =
  case D.decodeAvro sch bytes of
    Right val -> fromAvro val
    Left err  -> Error err

-- | Decode a container and de-conflict the writer schema with
-- a reader schema for a return type.
-- Like in 'decodeContainerWithSchema'
-- exceptions are thrown instead of a 'Result' type to
-- allow this function to be read lazy (to be done in some later version).
decodeContainer :: forall a. FromAvro a => ByteString -> [[a]]
decodeContainer bs =
  let readerSchema = untag (schema :: Tagged a Schema)
  in decodeContainerWithSchema readerSchema bs

-- |Decode a container and de-conflict the writer schema with a given
-- reader-schema.  Exceptions are thrown instead of a 'Result' type to
-- allow this function to be read lazy (to be done in some later version).
decodeContainerWithSchema :: FromAvro a => Schema -> ByteString -> [[a]]
decodeContainerWithSchema readerSchema bs =
  case D.decodeContainer bs of
    Right (writerSchema,val) ->
      let
        err e = error $ "Could not deconflict reader and writer schema." <> e
        dec x =
          case C.deconflict writerSchema readerSchema x of
            Left e   -> err e
            Right v  -> case fromAvro v of
                          Success x -> x
                          Error e   -> error e
      in P.map (P.map dec) val
    Left err -> error err

-- | Encodes a value to a lazy ByteString
encode :: ToAvro a => a -> BL.ByteString
encode = E.encodeAvro . toAvro

-- | Encode chunks of objects into a container, using 16 random bytes for
-- the synchronization markers.
encodeContainer :: forall a. ToAvro a => [[a]] -> IO BL.ByteString
encodeContainer = encodeContainer' nullCodec

encodeContainer' :: forall a. ToAvro a => Codec -> [[a]] -> IO BL.ByteString
encodeContainer' codec =
  let sch = untag (schema :: Tagged a Schema)
  in E.encodeContainer codec sch . map (map toAvro)

-- | Encode chunks of objects into a container, using the provided
-- ByteString as the synchronization markers.
encodeContainerWithSync :: forall a. ToAvro a => (Word64,Word64,Word64,Word64) -> [[a]] -> BL.ByteString
encodeContainerWithSync = encodeContainerWithSync' nullCodec

-- | Encode chunks of objects into a container, using the provided
-- ByteString as the synchronization markers.
encodeContainerWithSync' :: forall a. ToAvro a => Codec -> (Word64,Word64,Word64,Word64) -> [[a]] -> BL.ByteString
encodeContainerWithSync' codec (a,b,c,d) =
  let
    sch = untag (schema :: Tagged a Schema)
    syncBytes = P.runPut $ mapM_ P.putWord64le [a,b,c,d]
  in E.encodeContainerWithSync codec sch syncBytes . map (map toAvro)

-- |Like 'decodeContainer' but returns the avro-encoded bytes for each
-- object in the container instead of the Haskell type.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
decodeContainerBytes :: ByteString -> [[ByteString]]
decodeContainerBytes bs =
  case D.decodeContainerWith schemaBytes bs of
    Right (writerSchema, val) -> val
    Left e                    -> error $ "Could not decode container: " <> e
  where
  schemaBytes sch =
    do start <- G.bytesRead
       end   <- G.lookAhead $ do _ <- D.getAvroOf sch
                                 G.bytesRead
       G.getLazyByteString (end-start)

record :: Foldable f => Schema -> f (Text,T.Value Schema) -> T.Value Schema
record ty = T.Record ty . HashMap.fromList . toList

fixed :: Schema -> B.ByteString -> T.Value Schema
fixed = T.Fixed
-- @enumToAvro val@ will generate an Avro encoded value of enum suitable
-- for serialization ('encode').
-- enumToAvro :: (Show a, Enum a, Bounded a, Generic a) => a -> T.Value Schema
-- enumToAvro e = T.Enum ty (show e)
--  where
--   ty = S.Enum nm Nothing [] Nothing (map (Text.pack . show) [minBound..maxBound])
--   nm = datatypeName g
--   g  = from e -- GHC generics
