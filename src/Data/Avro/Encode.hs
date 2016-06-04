{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Avro.Encode
  ( PutAvro(..)
  ) where

import Data.Array (Array)
import Data.Ix (Ix)
import Data.Bits
import Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Builder
import qualified Data.Foldable as F
import Data.Int
import Data.Map
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as U
import Data.Word

import Data.Avro.Schema

-- XXX putContainer :: PutAvro a => Schema a -> a -> ByteString

class PutAvro a where
  putAvro :: a -> Builder

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

instance PutAvro Int  where
  putAvro = putIntegral
instance PutAvro Int8  where
  putAvro = putIntegral
instance PutAvro Int16  where
  putAvro = putIntegral
instance PutAvro Int32  where
  putAvro = putIntegral
instance PutAvro Int64  where
  putAvro = putIntegral
instance PutAvro Word8 where
  putAvro = putIntegral
instance PutAvro Word16 where
  putAvro = putIntegral
instance PutAvro Word32 where
  putAvro = putIntegral
instance PutAvro Word64 where
  putAvro = putIntegral
instance PutAvro Text where
  putAvro t =
    let bs = T.encodeUtf8 t
    in putIntegral (B.length bs) <> byteString bs
instance PutAvro TL.Text where
  putAvro t =
    let bs = TL.encodeUtf8 t
    in putIntegral (BL.length bs) <> lazyByteString bs

instance PutAvro String where
  putAvro s = let t = T.pack s in putAvro t

instance PutAvro a => PutAvro [a] where
  putAvro xs = putAvro (F.length xs) <> foldMap putAvro xs

instance (Ix i, PutAvro a) => PutAvro (Array i a) where
  putAvro a = putAvro (F.length a) <> foldMap putAvro a
instance PutAvro a => PutAvro (Vector a) where
  putAvro a = putAvro (F.length a) <> foldMap putAvro a
instance (U.Unbox a, PutAvro a) => PutAvro (U.Vector a) where
  putAvro a = putAvro (U.length a) <> foldMap putAvro (U.toList a)
instance PutAvro a => PutAvro (Set a) where
  putAvro a = putAvro (F.length a) <> foldMap putAvro a
-- XXX more from containers
-- XXX Unordered containers

instance (PutAvro a, PutAvro b) => PutAvro (a,b) where
  putAvro (a,b) = putAvro a <> putAvro b

instance (PutAvro a, PutAvro b, PutAvro c) => PutAvro (a,b,c) where
  putAvro (a,b,c) = putAvro a <> putAvro b <> putAvro c
instance (PutAvro a, PutAvro b, PutAvro c, PutAvro d) => PutAvro (a,b,c,d) where
  putAvro (a,b,c,d) = putAvro a <> putAvro b <> putAvro c <> putAvro d
instance (PutAvro a, PutAvro b, PutAvro c, PutAvro d, PutAvro e) => PutAvro (a,b,c,d,e) where
  putAvro (a,b,c,d,e) =
     putAvro a <> putAvro b <> putAvro c <> putAvro d <> putAvro e
instance (PutAvro a, PutAvro b, PutAvro c, PutAvro d, PutAvro e, PutAvro f) => PutAvro (a,b,c,d,e,f) where
  putAvro (a,b,c,d,e,f) =
    putAvro a <> putAvro b <> putAvro c <> putAvro d <> putAvro e <> putAvro f
instance (PutAvro a, PutAvro b, PutAvro c, PutAvro d, PutAvro e, PutAvro f, PutAvro g) => PutAvro (a,b,c,d,e,f,g) where
  putAvro (a,b,c,d,e,f,g) =
    putAvro a <> putAvro b <> putAvro c <> putAvro d <> putAvro e <> putAvro f <> putAvro g
instance (PutAvro a, PutAvro b, PutAvro c, PutAvro d, PutAvro e, PutAvro f, PutAvro g, PutAvro h) => PutAvro (a,b,c,d,e,f,g,h) where
  putAvro (a,b,c,d,e,f,g,h) =
    putAvro a <> putAvro b <> putAvro c <> putAvro d <> putAvro e <> putAvro f <> putAvro g <> putAvro h
instance (PutAvro a, PutAvro b, PutAvro c, PutAvro d, PutAvro e, PutAvro f, PutAvro g, PutAvro h, PutAvro i) => PutAvro (a,b,c,d,e,f,g,h,i) where
  putAvro (a,b,c,d,e,f,g,h,i) =
    putAvro a <> putAvro b <> putAvro c <> putAvro d <> putAvro e <> putAvro f <> putAvro g <> putAvro h <> putAvro i
instance (PutAvro a, PutAvro b, PutAvro c, PutAvro d, PutAvro e, PutAvro f, PutAvro g, PutAvro h, PutAvro i, PutAvro j) => PutAvro (a,b,c,d,e,f,g,h,i,j) where
  putAvro (a,b,c,d,e,f,g,h,i,j) =
    putAvro a <> putAvro b <> putAvro c <> putAvro d <> putAvro e <> putAvro f <> putAvro g <> putAvro h <> putAvro i <> putAvro j
instance (PutAvro a, PutAvro b, PutAvro c, PutAvro d, PutAvro e, PutAvro f, PutAvro g, PutAvro h, PutAvro i, PutAvro j, PutAvro k) => PutAvro (a,b,c,d,e,f,g,h,i,j,k) where
  putAvro (a,b,c,d,e,f,g,h,i,j,k) =
    putAvro a <> putAvro b <> putAvro c <> putAvro d <> putAvro e <> putAvro f <> putAvro g <> putAvro h <> putAvro i <> putAvro j <> putAvro k

-- | Maybe is modeled as a sum type `{null, a}`.
instance PutAvro a => PutAvro (Maybe a) where
  putAvro Nothing  = word8 0
  putAvro (Just x) = word8 1 <> putAvro x

instance PutAvro () where
  putAvro () = mempty

instance PutAvro Bool where
  putAvro = word8 . fromIntegral . fromEnum
