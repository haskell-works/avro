{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Avro.Decode.Get
where

import qualified Codec.Compression.Zlib     as Z
import           Control.Monad              (replicateM, when)
import qualified Data.Aeson                 as A
import qualified Data.Array                 as Array
import           Data.Binary.Get            (Get)
import qualified Data.Binary.Get            as G
import           Data.Binary.IEEE754        as IEEE
import           Data.Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Int
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
import           Data.Avro.DecodeRaw
import           Data.Avro.Schema           as S

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


data ContainerHeader = ContainerHeader
  { syncBytes       :: !BL.ByteString
  , decompress      :: forall a. Decompress a
  , containedSchema :: !Schema
  }

nrSyncBytes :: Integral sb => sb
nrSyncBytes = 16

instance GetAvro ContainerHeader where
  getAvro =
   do magic <- getFixed avroMagicSize
      when (BL.fromStrict magic /= avroMagicBytes)
           (fail "Invalid magic number at start of container.")
      metadata <- getMap :: Get (Map.Map Text BL.ByteString) -- avro.schema, avro.codec
      sync  <- BL.fromStrict <$> getFixed nrSyncBytes
      codec <- getCodec (Map.lookup "avro.codec" metadata)
      schema <- case Map.lookup "avro.schema" metadata of
                  Nothing -> fail "Invalid container object: no schema."
                  Just s  -> case A.eitherDecode' s of
                                Left e  -> fail ("Can not decode container schema: " <> e)
                                Right x -> return x
      return ContainerHeader { syncBytes = sync
                             , decompress = codecDecompress codec
                             , containedSchema = schema
                             }
   where avroMagicSize :: Integral a => a
         avroMagicSize = 4

         avroMagicBytes :: BL.ByteString
         avroMagicBytes = BC.pack "Obj" <> BL.pack [1]

         getFixed :: Int -> Get ByteString
         getFixed = G.getByteString


getCodec :: Monad m => Maybe BL.ByteString -> m Codec
getCodec (Just "null") = pure nullCodec
getCodec (Just "deflate") = pure deflateCodec
getCodec (Just x) = fail $ "Unrecognized codec: " <> BC.unpack x
getCodec Nothing = pure nullCodec


--------------------------------------------------------------------------------
--  Specialized Getters

getBoolean :: Get Bool
getBoolean =
 do w <- G.getWord8
    return $! (w == 0x01)

-- |Get a 32-bit int (zigzag encoded, max of 5 bytes)
getInt :: Get Int32
getInt = getZigZag

-- |Get a 64 bit int (zigzag encoded, max of 10 bytes)
getLong :: Get Int64
getLong = getZigZag

-- |Get an zigzag encoded integral value consuming bytes till the msb is 0.
getZigZag :: (Bits i, Integral i, DecodeRaw i) => Get i
getZigZag = decodeRaw

getBytes :: Get ByteString
getBytes =
 do w <- getLong
    G.getByteString (fromIntegral w)

getString :: Get Text
getString = do
  bytes <- getBytes
  case Text.decodeUtf8' bytes of
    Left unicodeExc -> fail (show unicodeExc)
    Right text      -> return text

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
getFloat = IEEE.wordToFloat <$> G.getWord32le

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
getDouble = IEEE.wordToDouble <$> G.getWord64le

--------------------------------------------------------------------------------
--  Complex AvroValue Getters

-- getRecord :: GetAvro ty => Get (AvroValue ty)
-- getRecord = getAvro

getArray :: GetAvro ty => Get [ty]
getArray = decodeBlocks getAvro

getMap :: GetAvro ty => Get (Map.Map Text ty)
getMap = Map.fromList <$> decodeBlocks keyValue
  where keyValue = (,) <$> getString <*> getAvro

-- | Avro encodes arrays and maps as a series of blocks. Each block
-- starts with a count of the elements in the block. A series of
-- blocks is always terminated with an empty block (encoded as a 0).
decodeBlocks :: Get a -> Get [a]
decodeBlocks element = do
  count <- getLong
  if | count == 0 -> return []

     -- negative counts are followed by the number of *bytes* in the
     -- array block
     | count < 0  -> do
         _bytes <- getLong
         items  <- replicateM (fromIntegral $ abs count) element'
         rest   <- decodeBlocks element
         pure $ items <> rest

     | otherwise  -> do
         items <- replicateM (fromIntegral count) element'
         rest  <- decodeBlocks element
         pure $ items <> rest
  where element' = do
          !x <- element
          pure x

-- Safe-ish from integral
sFromIntegral :: forall a b m. (Monad m, Bounded a, Bounded b, Integral a, Integral b) => a -> m b
sFromIntegral a
  | aI > fromIntegral (maxBound :: b) ||
    aI < fromIntegral (minBound :: b)   = fail "Integral overflow."
  | otherwise                           = return (fromIntegral a)
 where aI = fromIntegral a :: Integer
