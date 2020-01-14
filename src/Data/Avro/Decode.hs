{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Data.Avro.Decode
  ( decodeAvro
  , decodeContainer

  -- * Lower level interface
  , decodeContainerWith
  , getAvroOf
  , GetAvro(..)
  ) where

import qualified Codec.Compression.Zlib     as Z
import           Control.Monad              (replicateM, when)
import qualified Data.Aeson                 as A
import qualified Data.Array                 as Array
import           Data.Avro.Internal.Get
import           Data.Binary.Get            (Get, runGetOrFail)
import qualified Data.Binary.Get            as G
import           Data.Binary.IEEE754        as IEEE
import           Data.Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int
import           Data.List                  (foldl')
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Monoid                ((<>))
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Vector                as V
import           Prelude                    as P

import           Data.Avro.Codec
import           Data.Avro.Decode.Get
import           Data.Avro.DecodeRaw
import           Data.Avro.Schema     as S
import qualified Data.Avro.Types      as T
import           Data.Avro.Zag

import Data.Avro.Decode.Strict.Internal
import Data.Avro.Internal.Container     (ContainerHeader (..), getContainerHeader, nrSyncBytes)

-- | Decode bytes into a 'Value' as described by Schema.
decodeAvro :: Schema -> BL.ByteString -> Either String (T.Value Schema)
decodeAvro sch = either (\(_,_,s) -> Left s) (\(_,_,a) -> Right a) . runGetOrFail (getAvroOf sch)
{-# INLINABLE decodeAvro #-}

-- | Decode the container eagerly.
-- In order know whether to return an error or a successfully decoded value
-- the whole container is decoded into a memory.
--
-- "Data.Avro.Decode.Lazy" provides functions to decode Avro containers
-- in a lazy, streaming fashion.
decodeContainer :: BL.ByteString -> Either String (Schema, [[T.Value Schema]])
decodeContainer = decodeContainerWith getAvroOf
{-# INLINABLE decodeContainer #-}

-- | Decode container using a custom decoding function.
--
-- Honestly, I don't know why we still expose this function.
decodeContainerWith :: (Schema -> Get a)
                    -> BL.ByteString
                    -> Either String (Schema, [[a]])
decodeContainerWith schemaToGet bs =
  case runGetOrFail (getContainerWith schemaToGet) bs of
    Right (_,_,a) -> Right a
    Left (_,_,s)  -> Left s
{-# INLINABLE decodeContainerWith #-}

getContainerWith :: (Schema -> Get a) -> Get (Schema, [[a]])
getContainerWith schemaToGet =
   do ContainerHeader {..} <- getContainerHeader
      (containedSchema,) <$> getBlocks (schemaToGet containedSchema) syncBytes decompress
  where
  getBlocks :: Get a -> BL.ByteString -> (forall x. Decompress x) -> Get [[a]]
  getBlocks getValue sync decompress = do
    isEmpty <- G.isEmpty
    if isEmpty
      then return []
      else do
        nrObj    <- sFromIntegral =<< getLong
        nrBytes  <- getLong
        bytes    <- G.getLazyByteString nrBytes
        r        <- case decompress bytes (replicateM nrObj getValue) of
          Left err -> fail err
          Right x  -> pure x
        marker   <- G.getLazyByteString nrSyncBytes
        when (marker /= sync) (fail "Invalid marker, does not match sync bytes.")
        (r :) <$> getBlocks getValue sync decompress
