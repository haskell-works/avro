{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
module Data.Avro.Encode
  ( -- * High level interface
    getSchema
  , encodeAvro
  -- * Lower level interface
  , Avro(..)
  , putAvro
  ) where

import Prelude as P
import           Data.Array              (Array)
import           Data.Ix                 (Ix)
import           Data.Bits
import           Data.ByteString.Lazy    as BL
import qualified Data.ByteString         as B
import           Data.ByteString.Builder
import qualified Data.Foldable           as F
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Int
import           Data.List.NonEmpty      (NonEmpty(..))
import qualified Data.List.NonEmpty      as NE
import           Data.Monoid
import           Data.Set                (Set)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Vector             (Vector)
import qualified Data.Vector.Unboxed     as U
import           Data.Word
import           Data.Proxy

import Data.Avro.Schema as S
import Data.Avro.Types  as T

encodeAvro :: Avro a => a -> BL.ByteString
encodeAvro = toLazyByteString . putAvro

-- XXX make an instance 'Avro Schema'
-- Would require a schema schema...
-- encodeSchema :: Avro a => a -> BL.ByteString
-- encodeSchema = toLazyByteString . putAvro . getSchema

putAvro :: Avro a => a -> Builder
putAvro   = fst . runAvro . avro

getSchema :: forall a. Avro a => a -> Schema
getSchema _ = Schema (getType (Proxy :: Proxy a))

getType :: Avro a => Proxy a -> Type
getType p = snd (runAvro (avro (undefined `asProxyTypeOf` p)))
-- N.B. ^^^ Local knowledge that 'fst' won't be used,
-- so the bottom of 'undefined' will not escape so long as schema creation
-- remains lazy in the argument.

newtype AvroM = AvroM { runAvro :: (Builder,Type) }

class Avro a where
  avro :: a -> AvroM

-- | @putContainer schema obj@ Encodes the object and the schema for the
-- object into an Avro container as defined by the Avro specification.
-- XXX putContainer :: Avro a => Schema a -> a -> ByteString

-- class PutAvro a where
--   putAvro :: a -> Builder

avroInt :: forall a. (FiniteBits a, Integral a) => a -> AvroM
avroInt n = AvroM (putIntegral n, S.Int)

avroLong :: forall a. (FiniteBits a, Integral a) => a -> AvroM
avroLong n = AvroM (putIntegral n, S.Long)

-- Put a Haskell Int.
putI :: Int -> Builder
putI = putIntegral

putIntegral :: forall a. (FiniteBits a, Integral a) => a -> Builder
putIntegral n =
  let enc = (n `shiftL` 1) `xor` (n `shiftR` (finiteBitSize n - 1))
  in if enc == 0 then word8 0
                 else varEncode enc
 where
 varEncode :: a -> Builder
 varEncode 0 = mempty
 varEncode e =
  word8 (gt e (e .&. 0x7f)) <> varEncode (e `shiftR` 7)
 gt x | x >= 0x80 = (`setBit` 7) . fromIntegral
      | otherwise = fromIntegral

instance Avro Int  where
  avro = avroInt
instance Avro Int8  where
  avro = avroInt
instance Avro Int16  where
  avro = avroInt
instance Avro Int32  where
  avro = avroInt
instance Avro Int64  where
  avro = avroInt
instance Avro Word8 where
  avro = avroInt
instance Avro Word16 where
  avro = avroInt
instance Avro Word32 where
  avro = avroLong
instance Avro Word64 where
  avro = avroLong
instance Avro Text where
  avro t =
    let bs = T.encodeUtf8 t
    in AvroM (putIntegral (B.length bs) <> byteString bs, S.String)
instance Avro TL.Text where
  avro t =
    let bs = TL.encodeUtf8 t
    in AvroM (putIntegral (BL.length bs) <> lazyByteString bs, S.String)

instance Avro ByteString where
  avro bs = AvroM (putIntegral (BL.length bs) <> lazyByteString bs, S.Bytes)

instance Avro B.ByteString where
  avro bs = AvroM (putIntegral (B.length bs) <> byteString bs, S.Bytes)

instance Avro String where
  avro s = let t = T.pack s in avro t

instance Avro Double where
  avro d = AvroM (putIntegral longVal, S.Double)
   where longVal :: Word64
         longVal | isNaN d               = 0x7ff8000000000000
                 | isInfinite d && d > 0 = 0x7ff0000000000000
                 | isInfinite d          = 0xfff0000000000000
                 | otherwise = (s `shiftL` 63) .|. (e `shiftL` 52) .|. g
         s = fromIntegral (fromEnum (signum d < 0))
         e = fromIntegral (exponent d)
         g = floor (0x000fffffffffffff * significand d)

instance Avro Float where
  avro d = AvroM (putIntegral intVal, S.Float)
   where intVal :: Word32
         intVal | isNaN d               = 0x7fc00000
                | isInfinite d && d > 0 = 0x7f800000
                | isInfinite d          = 0xff800000
                | otherwise             = (s `shiftL` 31) .|. (e `shiftL` 23) .|. g
         s = fromIntegral (fromEnum (signum d < 0))
         e = fromIntegral (exponent d)
         g = floor (0x007fffff * significand d)

instance Avro a => Avro [a] where
  avro xs = AvroM ( putIntegral (F.length xs) <> foldMap putAvro xs
                  , S.Array (getType (Proxy :: Proxy a))
                  )

instance (Ix i, Avro a) => Avro (Array i a) where
  avro a = AvroM ( putIntegral (F.length a) <> foldMap putAvro a
                 , S.Array (getType (Proxy :: Proxy a))
                 )
instance Avro a => Avro (Vector a) where
  avro a = AvroM ( putIntegral (F.length a) <> foldMap putAvro a
                 , S.Array (getType (Proxy :: Proxy a))
                 )
instance (U.Unbox a, Avro a) => Avro (U.Vector a) where
  avro a = AvroM ( putIntegral (U.length a) <> foldMap putAvro (U.toList a)
                 , S.Array (getType (Proxy :: Proxy a))
                 )

instance Avro a => Avro (Set a) where
  avro a = AvroM ( putIntegral (F.length a) <> foldMap putAvro a
                 , S.Array (getType (Proxy :: Proxy a))
                 )

instance Avro a => Avro (HashMap Text a) where
  avro hm = AvroM ( putI (F.length hm) <> foldMap putKV (HashMap.toList hm)
                  , S.Map (getType (Proxy :: Proxy a))
                  )
    where putKV (k,v) = putAvro k <> putAvro v

-- XXX more from containers
-- XXX Unordered containers

-- | Maybe is modeled as a sum type `{null, a}`.
instance Avro a => Avro (Maybe a) where
  avro Nothing  = AvroM (putI 0             , S.Union (S.Null:|[S.Int]))
  avro (Just x) = AvroM (putI 1 <> putAvro x, S.Union (S.Null:|[S.Int]))

instance Avro () where
  avro () = AvroM (mempty, S.Null)

instance Avro Bool where
  avro b = AvroM (word8 $ fromIntegral $ fromEnum b, S.Boolean)

instance Avro (T.Value Type) where
  avro v =
    case v of
      T.Null      -> avro ()
      T.Boolean b -> avro b
      T.Int i     -> avro i
      T.Long i    -> avro i
      T.Float f   -> avro f
      T.Double d  -> avro d
      T.Bytes bs  -> avro bs
      T.String t  -> avro t
      T.Array vec -> avro vec
      T.Map hm    -> avro hm
      T.Record hm -> avro hm
      T.Union opts sel val | F.length opts > 0 ->
        case lookup sel (P.zip opts [0..]) of
          Just idx -> AvroM (putI idx <> putAvro val, S.Union (NE.fromList opts))
          Nothing  -> error "Union encoding specifies type not found in schema"
      T.Fixed bs  -> avro bs
      T.Enum sch@(S.Enum{..}) t ->
                case lookup t (P.zip symbols [0..]) of
                      Nothing -> error "Enum symbol not in schema."
                      Just ix -> AvroM ( putI ix
                                       , sch
                                       )
