{-# LANGUAGE TypeFamilies #-}

module Data.Avro.Internal.Zig
  ( Zig(..)
  ) where

import Data.Bits
import Data.Int
import Data.Word

class Zig a where
  type Zigged a
  zig :: a -> Zigged a

instance Zig Int8 where
  type Zigged Int8 = Word8
  zig n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` (finiteBitSize n - 1))
  {-# INLINE zig #-}

instance Zig Int16 where
  type Zigged Int16 = Word16
  zig n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` (finiteBitSize n - 1))
  {-# INLINE zig #-}

instance Zig Int32 where
  type Zigged Int32 = Word32
  zig n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` (finiteBitSize n - 1))
  {-# INLINE zig #-}

instance Zig Int64 where
  type Zigged Int64 = Word64
  zig n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` (finiteBitSize n - 1))
  {-# INLINE zig #-}

instance Zig Int where
  type Zigged Int = Word
  zig n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` (finiteBitSize n - 1))
  {-# INLINE zig #-}
