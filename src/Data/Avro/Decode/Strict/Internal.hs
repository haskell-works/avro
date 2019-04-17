{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Data.Avro.Decode.Strict.Internal
where

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
import           Data.Avro.Schema     as S
import qualified Data.Avro.Types      as T
import           Data.Avro.Zag

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
    Union ts ->
      do i <- getLong
         case ts V.!? (fromIntegral i) of
          Nothing -> fail $ "Decoded Avro tag is outside the expected range for a Union. Tag: " <> show i <> " union of: " <> show (V.map typeName ts)
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
