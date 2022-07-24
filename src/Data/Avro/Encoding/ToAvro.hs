{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Avro.Encoding.ToAvro
where

import           Control.Exception            (Exception(..), throw)
import           Control.Monad.Identity       (Identity (..))
import qualified Data.Array                   as Ar
import           Data.Avro.Internal.EncodeRaw
import           Data.Avro.Internal.Time
import           Data.Avro.Schema.Decimal     as D
import           Data.Avro.Schema.Schema      as S
import qualified Data.Binary.IEEE754          as IEEE
import qualified Data.ByteString              as B
import           Data.ByteString.Builder
import           Data.ByteString.Lazy         as BL
import qualified Data.Foldable                as F
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Int
import           Data.Ix                      (Ix)
import           Data.List                    as DL
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromJust)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
#if MIN_VERSION_text(2,0,0)
import qualified Data.Text.Foreign            as T
#endif
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TL
import           Data.Typeable                (Typeable(..), TypeRep, typeOf)
import qualified Data.Time                    as Time
import qualified Data.UUID                    as UUID
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import           Data.Word
import           GHC.Stack
import           GHC.TypeLits

{- HLINT ignore "Use section"         -}

newtype Encoder = Encoder { runEncoder :: Schema -> Builder }

(.=) :: forall a. ToAvro a => Text -> a -> (Text, Encoder)
(.=) fieldName fieldValue = (fieldName, Encoder (flip toAvro fieldValue))

record :: Schema -> [(Text, Encoder)] -> Builder
record (S.Record _ _ _ fs) vs =
  foldMap (mapField provided) fs
  where
    provided :: HashMap Text Encoder
    provided = HashMap.fromList vs

    providedNames = fst <$> vs

    failField :: S.Field -> Builder
    failField fld = error $ "Field '" <> show (S.fldName fld) <> "' is missing from the provided list of fields: " <> show providedNames

    mapField :: HashMap Text Encoder -> S.Field -> Builder
    mapField env fld =
      maybe (failField fld) (flip runEncoder (S.fldType fld)) (HashMap.lookup (S.fldName fld) env)
record s _ = error $ "Expected record, got: " <> show s

data AvroEncodingFailure = AvroEncodingFailure 
  { failedSchema :: Schema
  , failedType :: Maybe TypeRep
  , renderedValue :: Maybe String
  , failure :: Maybe String 
  }
  deriving (Show, Typeable)

failEncoding :: (HasCallStack, Typeable a, Show a) => Schema -> a -> b
failEncoding s x = throw $ AvroEncodingFailure s (Just $ typeOf x) (Just $ show x) Nothing

failEncodingWithMessage :: (HasCallStack, Typeable a, Show a) => Schema -> a -> String -> b
failEncodingWithMessage s x msg = throw $ AvroEncodingFailure s (Just $ typeOf x) (Just $ show x) $ Just msg

failEncodingWithoutTypeInfo :: (HasCallStack) => Schema -> String -> b
failEncodingWithoutTypeInfo s msg = throw $ AvroEncodingFailure s Nothing Nothing $ Just msg

instance Exception AvroEncodingFailure

-- | Describes how to encode Haskell data types into Avro bytes
class ToAvro a where
  toAvro :: Schema -> a -> Builder

instance ToAvro Int where
  toAvro (S.Long _) i = encodeRaw @Int64 (fromIntegral i)
  toAvro (S.Int _) i  = encodeRaw @Int32 (fromIntegral i)
  toAvro s i          = failEncoding s i
  {-# INLINE toAvro #-}

instance ToAvro Int32 where
  toAvro (S.Long _) i = encodeRaw @Int64 (fromIntegral i)
  toAvro (S.Int _) i  = encodeRaw @Int32 i
  toAvro S.Double i   = toAvro @Double S.Double (fromIntegral i)
  toAvro S.Float i    = toAvro @Float S.Float (fromIntegral i)
  toAvro s i          = failEncoding s i
  {-# INLINE toAvro #-}

instance ToAvro Int64 where
  toAvro (S.Long _) i = encodeRaw @Int64 i
  toAvro S.Double i   = toAvro @Double S.Double (fromIntegral i)
  toAvro S.Float i    = toAvro @Float S.Float (fromIntegral i)
  toAvro s i          = failEncoding s i
  {-# INLINE toAvro #-}

instance ToAvro Word8 where
  toAvro (S.Int _) i  = encodeRaw @Word8 i
  toAvro (S.Long _) i = encodeRaw @Word64 (fromIntegral i)
  toAvro S.Double i   = toAvro @Double S.Double (fromIntegral i)
  toAvro S.Float i    = toAvro @Float S.Float (fromIntegral i)
  toAvro s i          = failEncoding s i
  {-# INLINE toAvro #-}

instance ToAvro Word16 where
  toAvro (S.Int _) i  = encodeRaw @Word16 i
  toAvro (S.Long _) i = encodeRaw @Word64 (fromIntegral i)
  toAvro S.Double i   = toAvro @Double S.Double (fromIntegral i)
  toAvro S.Float i    = toAvro @Float S.Float (fromIntegral i)
  toAvro s i          = failEncoding s i
  {-# INLINE toAvro #-}

instance ToAvro Word32 where
  toAvro (S.Int _) i  = encodeRaw @Word32 i
  toAvro (S.Long _) i = encodeRaw @Word64 (fromIntegral i)
  toAvro S.Double i   = toAvro @Double S.Double (fromIntegral i)
  toAvro S.Float i    = toAvro @Float S.Float (fromIntegral i)
  toAvro s i          = failEncoding s i
  {-# INLINE toAvro #-}

instance ToAvro Word64 where
  toAvro (S.Long _) i = encodeRaw @Word64 i
  toAvro S.Double i   = toAvro @Double S.Double (fromIntegral i)
  toAvro s i          = failEncoding s i
  {-# INLINE toAvro #-}

instance ToAvro Double where
  toAvro S.Double i = word64LE (IEEE.doubleToWord i)
  toAvro s i        = failEncoding s i
  {-# INLINE toAvro #-}

instance ToAvro Float where
  toAvro S.Float i  = word32LE (IEEE.floatToWord i)
  toAvro S.Double i = word64LE (IEEE.doubleToWord $ realToFrac i)
  toAvro s i        = failEncoding s i
  {-# INLINE toAvro #-}

instance ToAvro () where
  toAvro S.Null () = mempty
  toAvro s ()      = failEncoding s ()

instance ToAvro Bool where
  toAvro S.Boolean v = word8 $ fromIntegral (fromEnum v)
  toAvro s v         = failEncoding s v
  {-# INLINE toAvro #-}

instance (KnownNat p, KnownNat s) => ToAvro (D.Decimal p s) where
  toAvro s = toAvro @Int64 s . fromIntegral . fromJust . D.underlyingValue

instance ToAvro UUID.UUID where
  toAvro s = toAvro s . UUID.toText
  {-# INLINE toAvro #-}

instance ToAvro Time.Day where
  toAvro s = toAvro @Int32 s . fromIntegral . daysSinceEpoch
  {-# INLINE toAvro #-}

instance ToAvro Time.DiffTime where
  toAvro s@(S.Long (Just S.TimeMicros))      = toAvro @Int64 s . fromIntegral . diffTimeToMicros
  toAvro s@(S.Long (Just S.TimestampMicros)) = toAvro @Int64 s . fromIntegral . diffTimeToMicros
  toAvro s@(S.Long (Just S.TimestampMillis)) = toAvro @Int64 s . fromIntegral . diffTimeToMillis
  toAvro s@(S.Int  (Just S.TimeMillis))      = toAvro @Int32 s . fromIntegral . diffTimeToMillis
  toAvro s                                   = failEncoding s

instance ToAvro Time.UTCTime where
  toAvro s@(S.Long (Just S.TimestampMicros)) = toAvro @Int64 s . fromIntegral . utcTimeToMicros
  toAvro s@(S.Long (Just S.TimestampMillis)) = toAvro @Int64 s . fromIntegral . utcTimeToMillis
  toAvro s                                   = failEncoding s

instance ToAvro Time.LocalTime where
  toAvro s@(S.Long (Just S.LocalTimestampMicros)) =
    toAvro @Int64 s . fromIntegral . localTimeToMicros
  toAvro s@(S.Long (Just S.LocalTimestampMillis)) =
    toAvro @Int64 s . fromIntegral . localTimeToMillis
  toAvro s = failEncoding s

instance ToAvro B.ByteString where
  toAvro s bs = case s of
    (S.Bytes _)                        -> encodeRaw (B.length bs) <> byteString bs
    (S.String _)                       -> encodeRaw (B.length bs) <> byteString bs
    S.Fixed _ _ l _ | l == B.length bs -> byteString bs
    S.Fixed _ _ l _                    -> failEncodingWithMessage s bs 
        ("Unable to encode ByteString as Fixed(" <> show l <> ") because its length is " <> show (B.length bs))
    _                                  -> failEncoding s bs
  {-# INLINE toAvro #-}

instance ToAvro BL.ByteString where
  toAvro s bs = toAvro s (BL.toStrict bs)
  {-# INLINE toAvro #-}

instance ToAvro Text where
  toAvro s v =
#if MIN_VERSION_text(2,0,0)
    let
      res =
        encodeRaw @Int64 (fromIntegral (T.lengthWord8 v)) <> T.encodeUtf8Builder v
     in case s of
       (S.Bytes _)  -> res
       (S.String _) -> res
       _            -> failEncoding s v
#else
    let
      bs = T.encodeUtf8 v
      res = encodeRaw (B.length bs) <> byteString bs
    in case s of
      (S.Bytes _)  -> res
      (S.String _) -> res
      _            -> failEncoding s v
#endif
  {-# INLINE toAvro #-}

instance ToAvro TL.Text where
  toAvro s v = toAvro s (TL.toStrict v)
  {-# INLINE toAvro #-}

instance ToAvro a => ToAvro [a] where
  toAvro (S.Array s) as =
    if DL.null as then long0 else encodeRaw (F.length as) <> foldMap (toAvro s) as <> long0
  toAvro s _            = failEncodingWithoutTypeInfo s "Unable to encode Haskell list"

instance ToAvro a => ToAvro (V.Vector a) where
  toAvro (S.Array s) as =
    if V.null as then long0 else encodeRaw (V.length as) <> foldMap (toAvro s) as <> long0
  toAvro s _            = failEncodingWithoutTypeInfo s "Unable to encode Vector"

instance (Ix i, ToAvro a) => ToAvro (Ar.Array i a) where
  toAvro (S.Array s) as =
    if F.length as == 0 then long0 else encodeRaw (F.length as) <> foldMap (toAvro s) as <> long0
  toAvro s _          = failEncodingWithoutTypeInfo s "Unable to encode Array"

instance (U.Unbox a, ToAvro a) => ToAvro (U.Vector a) where
  toAvro (S.Array s) as =
    if U.null as then long0 else encodeRaw (U.length as) <> foldMap (toAvro s) (U.toList as) <> long0
  toAvro s _          = failEncodingWithoutTypeInfo s "Unable to encode Unboxed Vector"

instance ToAvro a => ToAvro (Map.Map Text a) where
  toAvro (S.Map s) hm =
    if Map.null hm then long0 else putI (F.length hm) <> foldMap putKV (Map.toList hm) <> long0
    where putKV (k,v) = toAvro S.String' k <> toAvro s v
  toAvro s _          = failEncodingWithoutTypeInfo s "Unable to encode Map"

instance ToAvro a => ToAvro (HashMap Text a) where
  toAvro (S.Map s) hm =
    if HashMap.null hm then long0 else putI (F.length hm) <> foldMap putKV (HashMap.toList hm) <> long0
    where putKV (k,v) = toAvro S.String' k <> toAvro s v
  toAvro s _          = failEncodingWithoutTypeInfo s "Unable to encode HashMap"

instance ToAvro a => ToAvro (Maybe a) where
  toAvro (S.Union opts) v =
    case F.toList opts of
      [S.Null, s] -> maybe (putI 0) (\a -> putI 1 <> toAvro s a) v
      [s, S.Null] -> maybe (putI 1) (\a -> putI 0 <> toAvro s a) v
      wrongOpts   -> failEncodingWithoutTypeInfo (S.Union opts) "Unable to encode Maybe"
  toAvro s _ = failEncodingWithoutTypeInfo s "Unable to encode Maybe"

instance (ToAvro a) => ToAvro (Identity a) where
  toAvro (S.Union opts) e@(Identity a) =
    if V.length opts == 1
      then putI 0 <> toAvro (V.unsafeIndex opts 0) a
      else failEncodingWithoutTypeInfo (S.Union opts) ("Unable to encode Identity as a single-value union: " <> show opts)
  toAvro s _ = failEncodingWithoutTypeInfo s "Unable to encode Identity"

instance (ToAvro a, ToAvro b) => ToAvro (Either a b) where
  toAvro (S.Union opts) v =
    if V.length opts == 2
      then case v of
        Left a  -> putI 0 <> toAvro (V.unsafeIndex opts 0) a
        Right b -> putI 1 <> toAvro (V.unsafeIndex opts 1) b
      else failEncodingWithoutTypeInfo (S.Union opts) "Unable to encode Either as"
  toAvro s _ = failEncodingWithoutTypeInfo s "Unable to encode Either"
