{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Avro.Encoding.EncodeAvro
where

import           Control.Monad.Identity  (Identity (..))
import qualified Data.Array              as Ar
import           Data.Avro.EncodeRaw
import           Data.Avro.Internal.Time
import           Data.Avro.Schema        as S
import           Data.Avro.Types         as T
import           Data.Avro.Types.Decimal as D
import qualified Data.Binary.IEEE754     as IEEE
import qualified Data.ByteString         as B
import           Data.ByteString.Builder
import           Data.ByteString.Lazy    as BL
import qualified Data.Foldable           as F
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Int
import           Data.Ix                 (Ix)
import           Data.List               as DL
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromJust)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time               as Time
import qualified Data.UUID               as UUID
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as U
import           Data.Word
import           GHC.TypeLits

encodeAvro :: EncodeAvro a => Schema -> a -> BL.ByteString
encodeAvro s = toLazyByteString . toEncoding s
{-# INLINE encodeAvro #-}

class EncodeAvro a where
  toEncoding :: Schema -> a -> Builder

instance EncodeAvro Int where
  toEncoding (S.Long _) i = encodeRaw @Int64 (fromIntegral i)
  toEncoding (S.Int _) i  = encodeRaw @Int32 (fromIntegral i)
  toEncoding s _          = error ("Unable to encode Int as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro Int32 where
  toEncoding (S.Long _) i = encodeRaw @Int64 (fromIntegral i)
  toEncoding (S.Int _) i  = encodeRaw @Int32 i
  toEncoding s _          = error ("Unable to encode Int32 as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro Int64 where
  toEncoding (S.Long _) i = encodeRaw @Int64 i
  toEncoding s _          = error ("Unable to encode Int64 as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro Word8 where
  toEncoding (S.Int _) i  = encodeRaw @Word8 i
  toEncoding (S.Long _) i = encodeRaw @Word64 (fromIntegral i)
  toEncoding s _          = error ("Unable to encode Word8 as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro Word16 where
  toEncoding (S.Int _) i  = encodeRaw @Word16 i
  toEncoding (S.Long _) i = encodeRaw @Word64 (fromIntegral i)
  toEncoding s _          = error ("Unable to encode Word16 as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro Word32 where
  toEncoding (S.Int _) i  = encodeRaw @Word32 i
  toEncoding (S.Long _) i = encodeRaw @Word64 (fromIntegral i)
  toEncoding s _          = error ("Unable to encode Word32 as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro Word64 where
  toEncoding (S.Long _) i = encodeRaw @Word64 i
  toEncoding s _          = error ("Unable to encode Word64 as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro Double where
  toEncoding S.Double i = word64LE (IEEE.doubleToWord i)
  toEncoding s _        = error ("Unable to encode Double as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro Float where
  toEncoding S.Float i  = word32LE (IEEE.floatToWord i)
  toEncoding S.Double i = word64LE (IEEE.doubleToWord $ realToFrac i)
  toEncoding s _        = error ("Unable to encode Float as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro Bool where
  toEncoding S.Boolean v = word8 $ fromIntegral (fromEnum v)
  toEncoding s _         = error ("Unable to encode Bool as: " <> show s)
  {-# INLINE toEncoding #-}

instance (KnownNat p, KnownNat s) => EncodeAvro (D.Decimal p s) where
  toEncoding s = toEncoding @Int64 s . fromIntegral . fromJust . D.underlyingValue

instance EncodeAvro UUID.UUID where
  toEncoding s = toEncoding s . UUID.toText
  {-# INLINE toEncoding #-}

instance EncodeAvro Time.Day where
  toEncoding s = toEncoding @Int32 s . fromIntegral . daysSinceEpoch
  {-# INLINE toEncoding #-}

instance EncodeAvro Time.DiffTime where
  toEncoding s@(S.Long (Just S.TimeMicros))      = toEncoding @Int64 s . fromIntegral . diffTimeToMicros
  toEncoding s@(S.Long (Just S.TimestampMicros)) = toEncoding @Int64 s . fromIntegral . diffTimeToMicros
  toEncoding s@(S.Long (Just S.TimestampMillis)) = toEncoding @Int64 s . fromIntegral . diffTimeToMillis
  toEncoding s@(S.Int  (Just S.TimeMillis))      = toEncoding @Int32 s . fromIntegral . diffTimeToMillis
  toEncoding s                                   = error ("Unble to decode DiffTime from " <> show s)

instance EncodeAvro Time.UTCTime where
  toEncoding s@(S.Long (Just S.TimestampMicros)) = toEncoding @Int64 s . fromIntegral . utcTimeToMicros
  toEncoding s@(S.Long (Just S.TimestampMillis)) = toEncoding @Int64 s . fromIntegral . utcTimeToMillis

instance EncodeAvro B.ByteString where
  toEncoding s bs = case s of
    (S.Bytes _)                        -> encodeRaw (B.length bs) <> byteString bs
    S.Fixed _ _ l _ | l == B.length bs -> byteString bs
    S.Fixed _ _ l _                    -> error ("Unable to encode ByteString as Fixed(" <> show l <> ") because its length is " <> show (B.length bs))
    _                                  -> error ("Unable to encode ByteString as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro BL.ByteString where
  toEncoding s bs = toEncoding s (BL.toStrict bs)
  {-# INLINE toEncoding #-}

instance EncodeAvro Text where
  toEncoding s v =
    let
      bs = T.encodeUtf8 v
      res = encodeRaw (B.length bs) <> byteString bs
    in case s of
      (S.Bytes _)  -> res
      (S.String _) -> res
      _            -> error ("Unable to encode Text as: " <> show s)
  {-# INLINE toEncoding #-}

instance EncodeAvro TL.Text where
  toEncoding s v = toEncoding s (TL.toStrict v)
  {-# INLINE toEncoding #-}

instance EncodeAvro a => EncodeAvro [a] where
  toEncoding (S.Array s) as =
    if DL.null as then long0 else encodeRaw (F.length as) <> foldMap (toEncoding s) as <> long0
  toEncoding s _         = error ("Unable to encode Haskell list as: " <> show s)

instance EncodeAvro a => EncodeAvro (V.Vector a) where
  toEncoding (S.Array s) as =
    if V.null as then long0 else encodeRaw (V.length as) <> foldMap (toEncoding s) as <> long0
  toEncoding s _         = error ("Unable to encode Vector list as: " <> show s)

instance (Ix i, EncodeAvro a) => EncodeAvro (Ar.Array i a) where
  toEncoding (S.Array s) as =
    if F.length as == 0 then long0 else encodeRaw (F.length as) <> foldMap (toEncoding s) as <> long0
  toEncoding s _         = error ("Unable to encode indexed Array list as: " <> show s)

instance (U.Unbox a, EncodeAvro a) => EncodeAvro (U.Vector a) where
  toEncoding (S.Array s) as =
    if U.null as then long0 else encodeRaw (U.length as) <> foldMap (toEncoding s) (U.toList as) <> long0
  toEncoding s _         = error ("Unable to encode Vector list as: " <> show s)

instance EncodeAvro a => EncodeAvro (Map.Map Text a) where
  toEncoding (S.Map s) hm =
    if Map.null hm then long0 else putI (F.length hm) <> foldMap putKV (Map.toList hm) <> long0
    where putKV (k,v) = toEncoding S.String' k <> toEncoding s v
  toEncoding s _         = error ("Unable to encode HashMap as: " <> show s)

instance EncodeAvro a => EncodeAvro (HashMap Text a) where
  toEncoding (S.Map s) hm =
    if HashMap.null hm then long0 else putI (F.length hm) <> foldMap putKV (HashMap.toList hm) <> long0
    where putKV (k,v) = toEncoding S.String' k <> toEncoding s v
  toEncoding s _         = error ("Unable to encode HashMap as: " <> show s)

instance EncodeAvro a => EncodeAvro (Maybe a) where
  toEncoding (S.Union opts) v =
    case F.toList opts of
      [S.Null, s] -> maybe (putI 0) (\a -> putI 1 <> toEncoding s a) v
      wrongOpts   -> error ("Unable to encode Maybe as " <> show wrongOpts)
  toEncoding s _ = error ("Unable to encode Maybe as " <> show s)

instance (EncodeAvro a) => EncodeAvro (Identity a) where
  toEncoding (S.Union opts) e@(Identity a) =
    if (ivLength opts == 1)
      then putI 0 <> toEncoding (ivUnsafeIndex opts 0) a
      else error ("Unable to encode Identity as a single-value union: " <> show opts)
  toEncoding s _ = error ("Unable to encode Identity value as " <> show s)

instance (EncodeAvro a, EncodeAvro b) => EncodeAvro (Either a b) where
  toEncoding (S.Union opts) v =
    if (ivLength opts == 2)
      then case v of
        Left a  -> putI 0 <> toEncoding (ivUnsafeIndex opts 0) a
        Right b -> putI 1 <> toEncoding (ivUnsafeIndex opts 1) b
      else error ("Unable to encode Either as " <> show opts)
  toEncoding s _ = error ("Unable to encode Either as " <> show s)


-- Put a Haskell Int.
putI :: Int -> Builder
putI = encodeRaw
{-# INLINE putI #-}

-- Terminating word for array and map types.
long0 :: Builder
long0 = encodeRaw (0 :: Word64)
{-# INLINE long0 #-}
