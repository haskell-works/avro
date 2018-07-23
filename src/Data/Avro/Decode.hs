{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
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

import           Data.Avro.Decode.Get
import           Data.Avro.DecodeRaw
import           Data.Avro.Schema           as S
import qualified Data.Avro.Types            as T
import           Data.Avro.Zag

-- |Decode bytes into a 'Value' as described by Schema.
decodeAvro :: Schema -> BL.ByteString -> Either String (T.Value Type)
decodeAvro sch = either (\(_,_,s) -> Left s) (\(_,_,a) -> Right a) . runGetOrFail (getAvroOf sch)
{-# INLINABLE decodeAvro #-}

decodeContainer :: BL.ByteString -> Either String (Schema, [[T.Value Type]])
decodeContainer = decodeContainerWith getAvroOf
{-# INLINABLE decodeContainer #-}

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
   do ContainerHeader {..} <- getAvro
      (containedSchema,) <$> getBlocks (schemaToGet containedSchema) syncBytes decompress
  where
  getBlocks :: Get a -> BL.ByteString -> (BL.ByteString -> Get BL.ByteString) -> Get [[a]]
  getBlocks getValue sync decompress =
   do nrObj    <- sFromIntegral =<< getLong
      nrBytes  <- getLong
      bytes    <- decompress =<< G.getLazyByteString nrBytes
      r        <- case runGetOrFail (replicateM nrObj getValue) bytes of
                    Right (_,_,x) -> return x
                    Left (_,_,s)  -> fail s
      marker   <- G.getLazyByteString nrSyncBytes
      when (marker /= sync) (fail "Invalid marker, does not match sync bytes.")
      e <- G.isEmpty
      if e
        then return [r]
        else (r :) <$> getBlocks getValue sync decompress

getCodec :: Monad m => Maybe BL.ByteString -> m (BL.ByteString -> m BL.ByteString)
getCodec code | Just "null"    <- code =
                     return return
              | Just "deflate" <- code =
                     return (either (fail . show) return . Z.decompress)
              | Just x <- code =
                     fail ("Unrecognized codec: " <> BC.unpack x)
              | otherwise = return return

{-# INLINABLE getAvroOf #-}
getAvroOf :: Schema -> Get (T.Value Type)
getAvroOf ty0 = go ty0
 where
 env = S.buildTypeEnvironment envFail ty0
 envFail t = fail $ "Named type not in schema: " <> show t

 go :: Type -> Get (T.Value Type)
 go ty =
  case ty of
    Null    -> return T.Null
    Boolean -> T.Boolean <$> getAvro
    Int     -> T.Int     <$> getAvro
    Long    -> T.Long    <$> getAvro
    Float   -> T.Float   <$> getAvro
    Double  -> T.Double  <$> getAvro
    Bytes   -> T.Bytes   <$> getAvro
    String  -> T.String  <$> getAvro
    Array t ->
      do vals <- getBlocksOf t
         return $ T.Array (V.fromList $ mconcat vals)
    Map  t  ->
      do kvs <- getKVBlocks t
         return $ T.Map (HashMap.fromList $ mconcat kvs)
    NamedType tn -> env tn >>= go
    Record {..} ->
      do let getField Field {..} = (fldName,) <$> go fldType
         T.Record ty . HashMap.fromList <$> mapM getField fields
    Enum {..} ->
      do val <- getLong
         let sym = fromMaybe "" (symbolLookup val) -- empty string for 'missing' symbols (alternative is an error or exception)
         pure (T.Enum ty (fromIntegral val) sym)
    Union ts unionLookup ->
      do i <- getLong
         case unionLookup i of
          Nothing -> fail $ "Decoded Avro tag is outside the expected range for a Union. Tag: " <> show i <> " union of: " <> show (P.map typeName $ NE.toList ts)
          Just t  -> T.Union ts t <$> go t
    Fixed {..} -> T.Fixed ty <$> G.getByteString (fromIntegral size)

 getKVBlocks :: Type -> Get [[(Text,T.Value Type)]]
 getKVBlocks t =
  do blockLength <- abs <$> getLong
     if blockLength == 0
      then return []
      else do vs <- replicateM (fromIntegral blockLength) ((,) <$> getString <*> go t)
              (vs:) <$> getKVBlocks t
 {-# INLINE getKVBlocks #-}

 getBlocksOf :: Type -> Get [[T.Value Type]]
 getBlocksOf t =
  do blockLength <- abs <$> getLong
     if blockLength == 0
      then return []
      else do vs <- replicateM (fromIntegral blockLength) (go t)
              (vs:) <$> getBlocksOf t
 {-# INLINE getBlocksOf #-}
