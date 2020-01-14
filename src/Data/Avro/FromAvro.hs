{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Avro.FromAvro

where

import           Control.Arrow           (first)
import           Control.Monad.Identity  (Identity (..))
import qualified Data.Avro.Encode        as E
import           Data.Avro.HasAvroSchema
import           Data.Avro.Internal.Time
import           Data.Avro.Schema        as S
import           Data.Avro.Types         as T
import           Data.Avro.Types.Decimal as D
import qualified Data.ByteString         as B
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BL
import           Data.Foldable           (toList)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Int
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.Map                as Map
import           Data.Monoid             ((<>))
import           Data.Tagged
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as TL
import qualified Data.Time               as Time
import qualified Data.UUID               as UUID
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as U
import           Data.Word
import           GHC.TypeLits

class HasAvroSchema a => FromAvro a where
  fromAvro :: Value Schema -> Result a

(.:) :: FromAvro a => HashMap.HashMap Text (Value Schema) -> Text -> Result a
(.:) obj key =
  case HashMap.lookup key obj of
    Nothing -> fail $ "Requested field not available: " <> show key
    Just v  -> fromAvro v

instance (FromAvro a) => FromAvro (Identity a) where
  fromAvro e@(T.Union _ branch x)
    | S.matches branch sch = Identity <$> fromAvro x
    | otherwise            = badValue e "Identity"
    where Tagged sch = schema :: Tagged a Schema
  fromAvro x = badValue x "Identity"

instance (FromAvro a, FromAvro b) => FromAvro (Either a b) where
  fromAvro e@(T.Union _ branch x)
    | S.matches branch schemaA = Left  <$> fromAvro x
    | S.matches branch schemaB = Right <$> fromAvro x
    | otherwise              = badValue e "Either"
    where Tagged schemaA = schema :: Tagged a Schema
          Tagged schemaB = schema :: Tagged b Schema
  fromAvro x = badValue x "Either"

instance FromAvro Bool where
  fromAvro (T.Boolean b) = pure b
  fromAvro v             = badValue v "Bool"

instance FromAvro B.ByteString where
  fromAvro (T.Bytes _ b) = pure b
  fromAvro v             = badValue v "ByteString"

instance FromAvro BL.ByteString where
  fromAvro (T.Bytes _ b) = pure (BL.fromStrict b)
  fromAvro v             = badValue v "Lazy ByteString"

instance FromAvro Int where
  fromAvro (T.Int _ i) | (fromIntegral i :: Integer) < fromIntegral (maxBound :: Int)
                      = pure (fromIntegral i)
  fromAvro (T.Long _ i) | (fromIntegral i :: Integer) < fromIntegral (maxBound :: Int)
                      = pure (fromIntegral i)
  fromAvro v          = badValue v "Int"

instance FromAvro Int32 where
  fromAvro (T.Int _ i) = pure (fromIntegral i)
  fromAvro v           = badValue v "Int32"

instance FromAvro Int64 where
  fromAvro (T.Long _ i) = pure i
  fromAvro (T.Int _ i)  = pure (fromIntegral i)
  fromAvro v            = badValue v "Int64"

instance FromAvro Double where
  fromAvro (T.Double _ d) = pure d
  fromAvro v              = badValue v "Double"

instance FromAvro Float where
  fromAvro (T.Float _ f) = pure f
  fromAvro v             = badValue v "Float"

instance (KnownNat p, KnownNat s) => FromAvro (D.Decimal p s) where
  fromAvro (T.Long _ n) = pure $ D.fromUnderlyingValue $ fromIntegral n
  fromAvro (T.Int  _ n) = pure $ D.fromUnderlyingValue $ fromIntegral n
  fromAvro v            = badValue v "Decimal"

instance FromAvro UUID.UUID where
  fromAvro v@(T.String _ s)
    = case UUID.fromText s of
        Nothing -> badValue v "UUID"
        Just u  -> pure u
  fromAvro v = badValue v "UUID"

instance FromAvro Time.Day where
  fromAvro (T.Int  _ v) = pure $ fromDaysSinceEpoch (toInteger v)
  fromAvro (T.Long _ v) = pure $ fromDaysSinceEpoch (toInteger v)
  fromAvro v            = badValue v "Date"

instance FromAvro Time.DiffTime where
  fromAvro (T.Int  _ v) = pure $ microsToDiffTime (toInteger v)
  fromAvro (T.Long _ v) = pure $ microsToDiffTime (toInteger v)
  fromAvro v            = badValue v "TimeMicros"

instance FromAvro Time.UTCTime where
  fromAvro (T.Long _ v) = pure $ microsToUTCTime (toInteger v)
  fromAvro v            = badValue v "TimeMicros"

instance FromAvro a => FromAvro (Maybe a) where
  fromAvro (T.Union ts _ v) = case (V.toList ts, v) of
    ([S.Null, _], T.Null) -> pure Nothing
    ([S.Null, _], v')     -> Just <$> fromAvro v'
    _                     -> badValue v "Maybe a"
  fromAvro v                = badValue v "Maybe a"

instance FromAvro a => FromAvro [a] where
  fromAvro (T.Array vec) = mapM fromAvro $ toList vec
  fromAvro v             = badValue v "[a]"

instance FromAvro a => FromAvro (V.Vector a) where
  fromAvro (T.Array vec) = mapM fromAvro vec
  fromAvro v             = badValue v "Vector a"

instance (U.Unbox a, FromAvro a) => FromAvro (U.Vector a) where
  fromAvro (T.Array vec) = U.convert <$> mapM fromAvro vec
  fromAvro v             = badValue v "Unboxed Vector a"

instance FromAvro Text where
  fromAvro (T.String _ txt) = pure txt
  fromAvro v                = badValue v "Text"

instance FromAvro TL.Text where
  fromAvro (T.String _ txt) = pure (TL.fromStrict txt)
  fromAvro v                = badValue v "Lazy Text"

instance (FromAvro a) => FromAvro (Map.Map Text a) where
  fromAvro (T.Record _ mp) = mapM fromAvro $ Map.fromList (HashMap.toList mp)
  fromAvro (T.Map mp)      = mapM fromAvro $ Map.fromList (HashMap.toList mp)
  fromAvro v               = badValue v "Map Text a"

instance (FromAvro a) => FromAvro (HashMap.HashMap Text a) where
  fromAvro (T.Record _ mp) = mapM fromAvro mp
  fromAvro (T.Map mp)      = mapM fromAvro mp
  fromAvro v               = badValue v "HashMap Text a"
