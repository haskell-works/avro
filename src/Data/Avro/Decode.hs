{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
module Data.Avro.Decode
  ( decodeAvro
  , decodeContainer
  -- * Lower level interface
  , decodeContainerWith
  , getAvroOf
  , GetAvro(..)
  ) where

import Prelude as P
import           Control.Monad              (replicateM,when)
import qualified Codec.Compression.Zlib     as Z
import qualified Data.Aeson                 as A
import qualified Data.Array                 as Array
import qualified Data.Binary.Get            as G
import           Data.Binary.Get            (Get,runGetOrFail)
import           Data.Bits
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Int
import           Data.List                  (foldl')
import           Data.Monoid                ((<>))
import qualified Data.Map                   as Map
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Vector                as V

import           Data.Avro.Schema as S
import qualified Data.Avro.Types as T

-- |Decode bytes into a 'Value' as described by Schema.
decodeAvro :: Schema -> BL.ByteString -> Either String (T.Value Type)
decodeAvro sch = either (\(_,_,s) -> Left s) (\(_,_,a) -> Right a) . runGetOrFail (getAvroOf sch)

decodeContainer :: BL.ByteString -> Either String (Schema, [[T.Value Type]])
decodeContainer = decodeContainerWith getAvroOf

decodeContainerWith :: (Schema -> Get a)
                    -> BL.ByteString
                    -> Either String (Schema, [[a]])
decodeContainerWith schemaToGet bs =
  case runGetOrFail (getContainerWith schemaToGet) bs of
    Right (_,_,a) -> Right a
    Left (_,_,s)  -> Left s

getContainerWith :: (Schema -> Get a) -> Get (Schema, [[a]])
getContainerWith schemaToGet =
 do magic <- getFixed avroMagicSize
    when (BL.fromStrict magic /= avroMagicBytes)
         (fail "Invalid magic number at start of container.")
    metadata <- getMap :: Get (Map.Map Text BL.ByteString) -- avro.schema, avro.codec
    sync  <- BL.fromStrict <$> getFixed nrSyncBytes
    codec <- getCodec (Map.lookup "avro.codec" metadata)
    schema <- case Map.lookup "avro.schema" metadata of
                Nothing -> fail "Invalid container object: no schema."
                Just s  -> case A.eitherDecode' s of
                              Left e -> fail ("Can not decode container schema: " <> e)
                              Right (Schema x) -> return x
    (Schema schema,) <$> getBlocks (schemaToGet (Schema schema)) sync codec
  where
  nrSyncBytes :: Integral sb => sb
  nrSyncBytes = 16

  avroMagicSize :: Integral a => a
  avroMagicSize = 4

  avroMagicBytes :: BL.ByteString
  avroMagicBytes = BC.pack "Obj" <> BL.pack [1]

  getFixed :: Int -> Get ByteString
  getFixed = G.getByteString

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
                     return (maybe (fail "Decompression failed.") return . Z.decompress)
                | Just x <- code =
                     fail ("Unrecognized codec: " <> BC.unpack x)
                | otherwise = return return

getAvroOf :: Schema -> Get (T.Value Type)
getAvroOf (Schema ty0) = go ty0
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
      do let getField (Field {..}) = (fldName,) <$> go fldType
         T.Record . HashMap.fromList <$> mapM getField fields
    Enum {..} ->
      do val <- getLong
         let resolveEnum = flip lookup (zip [0..] symbols)
         case resolveEnum val of
          Just e  -> return (T.Enum ty e)
          Nothing -> fail $ "Decoded Avro enumeration is outside the expected range. Value: " <> show val <> " enum name: " <> show name
    Union ts ->
      do i <- getLong
         let resolveUnion = flip lookup (zip [0..] ts)
         case resolveUnion i of
          Nothing -> fail $ "Decoded Avro tag is outside the expected range for a Union. Tag: " <> show i <> " union of: " <> show (P.map typeName ts)
          Just t  -> T.Union ts t <$> go t
    Fixed {..} -> T.Fixed <$> G.getByteString (fromIntegral size)

 getKVBlocks :: Type -> Get [[(Text,T.Value Type)]]
 getKVBlocks t =
  do blockLength <- abs <$> getLong
     if blockLength == 0
      then return []
      else do vs <- replicateM (fromIntegral blockLength) ((,) <$> getString <*> go t)
              (vs:) <$> getKVBlocks t
 getBlocksOf :: Type -> Get [[T.Value Type]]
 getBlocksOf t =
  do blockLength <- abs <$> getLong
     if blockLength == 0
      then return []
      else do vs <- replicateM (fromIntegral blockLength) (go t)
              (vs:) <$> getBlocksOf t


class GetAvro a where
  getAvro :: Get a

instance GetAvro ty => GetAvro (Map.Map Text ty) where
  getAvro = getMap
instance GetAvro Bool where
  getAvro = getBoolean
instance GetAvro Int32 where
  getAvro = getInt
instance GetAvro Int64 where
  getAvro = getLong
instance GetAvro BL.ByteString where
  getAvro = BL.fromStrict <$> getBytes
instance GetAvro ByteString where
  getAvro = getBytes
instance GetAvro Text where
  getAvro = getString
instance GetAvro Float where
  getAvro = getFloat
instance GetAvro Double where
  getAvro = getDouble
instance GetAvro String where
  getAvro = Text.unpack <$> getString
instance GetAvro a => GetAvro [a] where
  getAvro = getArray
instance GetAvro a => GetAvro (Maybe a) where
  getAvro =
    do t <- getLong
       case t of
        0 -> return Nothing
        1 -> Just <$> getAvro
        n -> fail $ "Invalid tag for expected {null,a} Avro union, received: " <> show n


instance GetAvro a => GetAvro (Array.Array Int a) where
  getAvro =
    do ls <- getAvro
       return $ Array.listArray (0,length ls - 1) ls
instance GetAvro a => GetAvro (V.Vector a) where
  getAvro = V.fromList <$> getAvro
instance (GetAvro a, Ord a) => GetAvro (Set.Set a) where
  getAvro = Set.fromList <$> getAvro

--------------------------------------------------------------------------------
--  Specialized Getters

getBoolean :: Get Bool
getBoolean =
 do w <- G.getWord8
    return (w == 0x01)

-- |Get a 32-bit int (zigzag encoded, max of 5 bytes)
getInt :: Get Int32
getInt = getZigZag

-- |Get a 64 bit int (zigzag encoded, max of 10 bytes)
getLong :: Get Int64
getLong = getZigZag

-- |Get an zigzag encoded integral value consuming bytes till the msb is 0.
getZigZag :: (Bits i, Integral i) => Get i
getZigZag =
  do orig <- getWord8s
     let word0 = foldl' (\a x -> (a `shiftL` 7) + fromIntegral x) 0 (reverse orig)
     return ((word0 `shiftR` 1) `xor` (negate (word0  .&. 1) ))
 where
   getWord8s =
    do w <- G.getWord8
       let msb = w `testBit` 7
       (w .&. 0x7F :) <$> if msb then getWord8s
                                 else return []

getBytes :: Get ByteString
getBytes =
 do w <- getLong
    G.getByteString (fromIntegral w)

getString :: Get Text
getString = Text.decodeUtf8 <$> getBytes

-- a la Java:
--  Bit 31 (the bit that is selected by the mask 0x80000000) represents the
--  sign of the floating-point number. Bits 30-23 (the bits that are
--  selected by the mask 0x7f800000) represent the exponent. Bits 22-0 (the
--  bits that are selected by the mask 0x007fffff) represent the
--  significand (sometimes called the mantissa) of the floating-point
--  number.
--
--  If the argument is positive infinity, the result is 0x7f800000.
--
--  If the argument is negative infinity, the result is 0xff800000.
--
--  If the argument is NaN, the result is 0x7fc00000. 
getFloat :: Get Float
getFloat =
 do f <- G.getWord32le
    let dec | f == 0x7f800000 = 1 / 0
            | f == 0xff800000 = negate 1 /0
            | f == 0x7fc00000 = 0 / 0
            | otherwise =
                let s = if f .&. 0x80000000 == 0 then id else negate
                    e = (f .&. 0x7f800000) `shiftR` 23
                    m = f .&. 0x007fffff
                in s (fromIntegral m * 2^e)
    return dec

-- As in Java:
--  Bit 63 (the bit that is selected by the mask 0x8000000000000000L)
--  represents the sign of the floating-point number. Bits 62-52 (the bits
--  that are selected by the mask 0x7ff0000000000000L) represent the
--  exponent. Bits 51-0 (the bits that are selected by the mask
--  0x000fffffffffffffL) represent the significand (sometimes called the
--  mantissa) of the floating-point number.
--
--  If the argument is positive infinity, the result is
--  0x7ff0000000000000L.
--
--  If the argument is negative infinity, the result is
--  0xfff0000000000000L.
--
--  If the argument is NaN, the result is 0x7ff8000000000000L
getDouble :: Get Double
getDouble =
 do f <- G.getWord64le
    let dec | f == 0x7ff0000000000000 = 1 / 0
            | f == 0xfff0000000000000 = negate 1 / 0
            | f == 0x7ff8000000000000 = 0 / 0
            | otherwise =
                let s = if f .&. 0x8000000000000000 == 0 then id else negate
                    e = (f .&. 0x7ff0000000000000) `shiftR` 52
                    m = f .&. 0x000fffffffffffff
                in s (fromIntegral m * 2^e)
    return dec

--------------------------------------------------------------------------------
--  Complex AvroValue Getters

-- getRecord :: GetAvro ty => Get (AvroValue ty)
-- getRecord = getAvro

getArray :: GetAvro ty => Get [ty]
getArray =
  do nr <- getLong
     if
      | nr == 0 -> return []
      | nr < 0  ->
          do _len <- getLong
             rs <- replicateM (fromIntegral (abs nr)) getAvro
             (rs <>) <$> getArray
      | otherwise ->
          do rs <- replicateM (fromIntegral nr) getAvro
             (rs <>) <$> getArray

getMap :: GetAvro ty => Get (Map.Map Text ty)
getMap = go Map.empty
 where
 go acc =
  do nr <- getLong
     if nr == 0
       then return acc
       else do m <- Map.fromList <$> replicateM (fromIntegral nr) getKVs
               go (Map.union m acc)
 getKVs = (,) <$> getString <*> getAvro

-- Safe-ish from integral
sFromIntegral :: forall a b m. (Monad m, Bounded a, Bounded b, Integral a, Integral b) => a -> m b
sFromIntegral a
  | aI > fromIntegral (maxBound :: b) ||
    aI < fromIntegral (minBound :: b)   = fail "Integral overflow."
  | otherwise                           = return (fromIntegral a)
 where aI = fromIntegral a :: Integer
