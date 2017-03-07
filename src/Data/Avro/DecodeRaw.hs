module Data.Avro.DecodeRaw
  ( DecodeRaw(..)
  ) where

import Data.Avro.Zag
import Data.Binary.Get
import Data.Bits
import Data.Int
import Data.List
import Data.Word

getNonNegative :: (Bits i, Integral i) => Get i
getNonNegative = do
  orig <- getWord8s
  return (foldl' (\a x -> (a `shiftL` 7) + fromIntegral x) 0 (reverse orig))

getWord8s :: Get [Word8]
getWord8s = do
  w <- getWord8
  let msb = w `testBit` 7 in (w .&. 0x7F :) <$> if msb
    then getWord8s
    else return []

class DecodeRaw a where
  decodeRaw :: Get a

instance DecodeRaw Word where
  decodeRaw = getNonNegative
  {-# INLINE decodeRaw #-}

instance DecodeRaw Word8 where
  decodeRaw = getNonNegative
  {-# INLINE decodeRaw #-}

instance DecodeRaw Word16 where
  decodeRaw = getNonNegative
  {-# INLINE decodeRaw #-}

instance DecodeRaw Word32 where
  decodeRaw = getNonNegative
  {-# INLINE decodeRaw #-}

instance DecodeRaw Word64 where
  decodeRaw = getNonNegative
  {-# INLINE decodeRaw #-}

instance DecodeRaw Int where
  decodeRaw = zag <$> (decodeRaw :: Get Word)
  {-# INLINE decodeRaw #-}

instance DecodeRaw Int8 where
  decodeRaw = zag <$> (decodeRaw :: Get Word8)
  {-# INLINE decodeRaw #-}

instance DecodeRaw Int16 where
  decodeRaw = zag <$> (decodeRaw :: Get Word16)
  {-# INLINE decodeRaw #-}

instance DecodeRaw Int32 where
  decodeRaw = zag <$> (decodeRaw :: Get Word32)
  {-# INLINE decodeRaw #-}

instance DecodeRaw Int64 where
  decodeRaw = zag <$> (decodeRaw :: Get Word64)
  {-# INLINE decodeRaw #-}
