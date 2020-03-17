{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE StrictData        #-}

module Data.Avro.Encoding.Value where

import           Control.DeepSeq             (NFData)
import           Control.Monad.Identity      (Identity (..))
import           Data.Avro.Internal.Time
import           Data.Avro.Schema.ReadSchema (ReadSchema)
import qualified Data.Avro.Schema.ReadSchema as Schema
import qualified Data.Avro.Types             as T
import           Data.Avro.Types.Decimal     as D
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Int
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.Map                    as Map
import           Data.Text
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Time                   as Time
import qualified Data.UUID                   as UUID
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           GHC.Generics                (Generic)
import           GHC.TypeLits

data Value
      = Null
      | Boolean Bool
      | Int     ReadSchema {-# UNPACK #-} Int32
      | Long    ReadSchema {-# UNPACK #-} Int64
      | Float   ReadSchema {-# UNPACK #-} Float
      | Double  ReadSchema {-# UNPACK #-} Double
      | Bytes   ReadSchema {-# UNPACK #-} BS.ByteString
      | String  ReadSchema {-# UNPACK #-} Text
      | Array   (Vector Value)
      | Map     (HashMap Text Value)
      | Record  (Vector Value)
      | Union   ReadSchema {-# UNPACK #-} Int Value
      | Fixed   ReadSchema {-# UNPACK #-} BS.ByteString
      | Enum    ReadSchema {-# UNPACK #-} Int {-# UNPACK #-} Text
  deriving (Eq, Show, Generic, NFData)

describeValue :: Value -> String
describeValue = \case
  Null          -> "Null"
  Boolean b     -> "Boolean"
  Int s _       -> "Int (" <> show s <> ")"
  Long s _      -> "Long (" <> show s <> ")"
  Float s _     -> "Float (" <> show s <> ")"
  Double s _    -> "Double (" <> show s <> ")"
  Bytes s _     -> "Bytes (" <> show s <> ")"
  String s _    -> "String (" <> show s <> ")"
  Union s ix _  -> "Union (position = " <> show ix <> ", schema = " <> show s <> ")"
  Fixed s _     -> "Fixed (" <> show s <> ")"
  Enum s ix _   -> "Enum (position = " <> show ix <> ", schema =" <> show s <> ")"
  Array vs      -> "Array (length = " <> show (V.length vs) <> ")"
  Map vs        -> "Map (length = " <> show (HashMap.size vs) <> ")"
  Record vs     -> "Record (fieldsNum = " <> show (V.length vs) <> ")"

--------------------------------------------------------------------------
class DecodeAvro a where
  fromValue :: Value -> Either String a

instance DecodeAvro Int where
  fromValue (Int _ x)  = Right (fromIntegral x)
  fromValue (Long _ x) = Right (fromIntegral x)
  fromValue x          = Left ("Unable decode Int from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Int32 where
  fromValue (Int _ x) = Right x
  fromValue x         = Left ("Unable decode Int32 from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Int64 where
  fromValue (Long _ x) = Right x
  fromValue (Int _ x)  = Right (fromIntegral x)
  fromValue x          = Left ("Unable decode Int64 from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Double where
  fromValue (Double _ x) = Right x
  fromValue (Float _ x)  = Right (realToFrac x)
  fromValue (Long _ x)   = Right (fromIntegral x)
  fromValue (Int _ x)    = Right (fromIntegral x)
  fromValue x            = Left ("Unable decode Double from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Float where
  fromValue (Float _ x) = Right x
  fromValue (Long _ x)  = Right (fromIntegral x)
  fromValue (Int _ x)   = Right (fromIntegral x)
  fromValue x           = Left ("Unable decode Double from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Bool where
  fromValue (Boolean x) = Right x
  fromValue x           = Left ("Unable decode Bool from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Text where
  fromValue (String _ x) = Right x
  fromValue (Bytes _ x) = case Text.decodeUtf8' x of
    Left unicodeExc -> Left (show unicodeExc)
    Right text      -> Right text
  fromValue x          = Left ("Unable decode Text from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro BS.ByteString where
  fromValue (Bytes _ x)  = Right x
  fromValue (String _ x) = Right (Text.encodeUtf8 x)
  fromValue x            = Left ("Unable to decode Bytes from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro BL.ByteString where
  fromValue (Bytes _ bs) = Right (BL.fromStrict bs)
  fromValue (String _ x) = Right (BL.fromStrict $ Text.encodeUtf8 x)
  fromValue x            = Left ("Unable decode Bytes from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance (KnownNat p, KnownNat s) => DecodeAvro (D.Decimal p s) where
  fromValue (Long _ n) = Right $ D.fromUnderlyingValue $ fromIntegral n
  fromValue (Int _ n)  = Right $ D.fromUnderlyingValue $ fromIntegral n
  fromValue x          = Left ("Unable to decode Decimal from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro UUID.UUID where
  fromValue (String _ x) =
    case UUID.fromText x of
      Nothing -> Left "Unable to UUID from a given String value"
      Just u  -> Right u
  fromValue x            = Left ("Unable to decode UUID from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Time.Day where
  fromValue (Int (Schema.Int (Just Schema.Date)) n) = Right $ fromDaysSinceEpoch (toInteger n)
  fromValue x                                       = Left ("Unable to decode Day from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Time.DiffTime where
  fromValue (Int (Schema.Int (Just Schema.TimeMillis)) n)          = Right $ millisToDiffTime (toInteger n)
  fromValue (Long (Schema.Long _ (Just Schema.TimestampMillis)) n) = Right $ millisToDiffTime (toInteger n)
  fromValue (Long (Schema.Long _ (Just Schema.TimeMicros)) n)      = Right $ microsToDiffTime (toInteger n)
  fromValue (Long (Schema.Long _ (Just Schema.TimestampMicros)) n) = Right $ microsToDiffTime (toInteger n)
  fromValue x                                                      = Left ("Unable to decode TimeDiff from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Time.UTCTime where
  fromValue (Long (Schema.Long _ (Just Schema.TimestampMicros)) n) = Right $ microsToUTCTime (toInteger n)
  fromValue (Long (Schema.Long _ (Just Schema.TimestampMillis)) n) = Right $ millisToUTCTime (toInteger n)
  fromValue x                                                      = Left ("Unable to decode UTCTime from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro a => DecodeAvro [a] where
  fromValue (Array vec) = mapM fromValue $ V.toList vec
  fromValue x           = Left ("Unable to decode Array from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro a => DecodeAvro (Identity a) where
  fromValue (Union _ 0 v) = Identity <$> fromValue v
  fromValue (Union _ n _) = Left ("Unable to decode Identity value from value with a position #" <> show n)
  fromValue x             = Left ("Unable to decode Identity from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro a => DecodeAvro (Maybe a) where
  fromValue (Union _ _ Null) = Right Nothing
  fromValue (Union _ _ v)    = Just <$> fromValue v
  fromValue x                = Left ("Unable to decode Maybe from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance (DecodeAvro a, DecodeAvro b) => DecodeAvro (Either a b) where
  fromValue (Union _ 0 a) = Left <$> fromValue a
  fromValue (Union _ 1 b) = Right <$> fromValue b
  fromValue (Union _ n _) = Left ("Unable to decode union value with a position #" <> show n)
  fromValue x             = Left ("Unable to decode Either from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro a => DecodeAvro (Map.Map Text a) where
  fromValue (Map mp) = traverse fromValue (Map.fromList (HashMap.toList mp))
  fromValue x        = Left ("Unable to decode Map from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro a => DecodeAvro (HashMap.HashMap Text a) where
  fromValue (Map mp) = traverse fromValue mp
  fromValue x        = Left ("Unable to decode Map from: " <> show (describeValue x))
  {-# INLINE fromValue #-}
