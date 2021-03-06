{-# LANGUAGE ScopedTypeVariables #-}

module Data.Avro.Internal.EncodeRaw
  ( EncodeRaw(..)
  , putI
  , long0
  ) where

import Data.Avro.Internal.Zig
import Data.Bits
import Data.ByteString.Builder
import Data.Int
import Data.Word

putNonNegative :: forall a. (FiniteBits a, Integral a) => a -> Builder
putNonNegative n = if n .&. complement 0x7F == 0
  then word8 $ fromIntegral (n .&. 0x7f)
  else word8 (0x80 .|. (fromIntegral n .&. 0x7F)) <> putNonNegative (n `shiftR` 7)

class EncodeRaw a where
  encodeRaw :: a -> Builder

instance EncodeRaw Word where
  encodeRaw = putNonNegative
  {-# INLINE encodeRaw #-}

instance EncodeRaw Word8 where
  encodeRaw = putNonNegative
  {-# INLINE encodeRaw #-}

instance EncodeRaw Word16 where
  encodeRaw = putNonNegative
  {-# INLINE encodeRaw #-}

instance EncodeRaw Word32 where
  encodeRaw = putNonNegative
  {-# INLINE encodeRaw #-}

instance EncodeRaw Word64 where
  encodeRaw = putNonNegative
  {-# INLINE encodeRaw #-}

instance EncodeRaw Int where
  encodeRaw = encodeRaw . zig
  {-# INLINE encodeRaw #-}

instance EncodeRaw Int8 where
  encodeRaw = encodeRaw . zig
  {-# INLINE encodeRaw #-}

instance EncodeRaw Int16 where
  encodeRaw = encodeRaw . zig
  {-# INLINE encodeRaw #-}

instance EncodeRaw Int32 where
  encodeRaw = encodeRaw . zig
  {-# INLINE encodeRaw #-}

instance EncodeRaw Int64 where
  encodeRaw = encodeRaw . zig
  {-# INLINE encodeRaw #-}

-- Put a Haskell Int.
putI :: Int -> Builder
putI = encodeRaw
{-# INLINE putI #-}

-- Terminating word for array and map types.
long0 :: Builder
long0 = encodeRaw (0 :: Word64)
{-# INLINE long0 #-}
