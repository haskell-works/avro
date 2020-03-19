{-# LANGUAGE TypeFamilies #-}

module Data.Avro.Internal.Zag
  ( Zag(..)
  ) where

import Data.Bits
import Data.Int
import Data.Word

class Zag a where
  type Zagged a
  zag :: a -> Zagged a

instance Zag Word8 where
  type Zagged Word8 = Int8
  zag n = fromIntegral $ (n `shiftR` 1) `xor` negate (n .&. 0x1)
  {-# INLINE zag #-}

instance Zag Word16 where
  type Zagged Word16 = Int16
  zag n = fromIntegral $ (n `shiftR` 1) `xor` negate (n .&. 0x1)
  {-# INLINE zag #-}

instance Zag Word32 where
  type Zagged Word32 = Int32
  zag n = fromIntegral $ (n `shiftR` 1) `xor` negate (n .&. 0x1)
  {-# INLINE zag #-}

instance Zag Word64 where
  type Zagged Word64 = Int64
  zag n = fromIntegral $ (n `shiftR` 1) `xor` negate (n .&. 0x1)
  {-# INLINE zag #-}

instance Zag Word where
  type Zagged Word = Int
  zag n = fromIntegral $ (n `shiftR` 1) `xor` negate (n .&. 0x1)
  {-# INLINE zag #-}
