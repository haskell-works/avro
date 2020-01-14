{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Avro.ToAvro

where

import           Control.Arrow           (first)
import           Control.Monad.Identity  (Identity (..))
import           Data.Avro.HasAvroSchema
import           Data.Avro.Internal.Time
import           Data.Avro.Schema        as S
import           Data.Avro.Types         as T
import           Data.Avro.Types.Decimal as D
import qualified Data.ByteString         as B
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as HashMap
import           Data.Int
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust)
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

class HasAvroSchema a => ToAvro a where
  toAvro :: a -> T.Value Schema

(.=)  :: ToAvro a => Text -> a -> (Text,T.Value Schema)
(.=) nm val = (nm,toAvro val)

instance ToAvro Bool where
  toAvro = T.Boolean

instance ToAvro () where
  toAvro _ = T.Null

instance ToAvro Int where
  toAvro = T.Long S.Long' . fromIntegral

instance ToAvro Int32 where
  toAvro = T.Int S.Int'

instance ToAvro Int64 where
  toAvro = T.Long S.Long'

instance ToAvro Double where
  toAvro = T.Double S.Double

instance ToAvro Float where
  toAvro = T.Float S.Float

instance ToAvro Text.Text where
  toAvro = T.String S.String'

instance ToAvro TL.Text where
  toAvro = T.String S.String' . TL.toStrict

instance ToAvro B.ByteString where
  toAvro = T.Bytes S.Bytes'

instance ToAvro BL.ByteString where
  toAvro = T.Bytes S.Bytes' . BL.toStrict

instance (KnownNat p, KnownNat s) => ToAvro (D.Decimal p s) where
  toAvro d = T.Long (schemaOf d) $ (fromIntegral . fromJust . D.underlyingValue) d

instance ToAvro UUID.UUID where
  toAvro = T.String (S.String $ Just S.UUID) . UUID.toText

instance ToAvro Time.Day where
  toAvro = T.Long (S.Long $ Just S.TimestampMicros) . fromIntegral . daysSinceEpoch

instance ToAvro Time.DiffTime where
  toAvro = T.Long (S.Long $ Just S.TimestampMicros) . fromIntegral . diffTimeToMicros

instance ToAvro Time.UTCTime where
  toAvro = T.Long (S.Long $ Just S.TimestampMicros) . fromIntegral . utcTimeToMicros

instance (ToAvro a) => ToAvro (Identity a) where
  toAvro e@(Identity a) =
    let sch = extractValues $ options (schemaOf e)
    in
      T.Union sch (schemaOf a) (toAvro a)

instance (ToAvro a, ToAvro b) => ToAvro (Either a b) where
  toAvro e =
    let sch = extractValues $ options (schemaOf e)
    in case e of
         Left a  -> T.Union sch (schemaOf a) (toAvro a)
         Right b -> T.Union sch (schemaOf b) (toAvro b)

instance (ToAvro a) => ToAvro (Map.Map Text a) where
  toAvro = toAvro . HashMap.fromList . Map.toList

instance (ToAvro a) => ToAvro (HashMap.HashMap Text a) where
  toAvro = T.Map . HashMap.map toAvro

instance (ToAvro a) => ToAvro (Map.Map TL.Text a) where
  toAvro = toAvro . HashMap.fromList . map (first TL.toStrict) . Map.toList

instance (ToAvro a) => ToAvro (HashMap.HashMap TL.Text a) where
  toAvro = toAvro . HashMap.fromList . map (first TL.toStrict) . HashMap.toList

instance (ToAvro a) => ToAvro (Map.Map String a) where
  toAvro = toAvro . HashMap.fromList . map (first Text.pack) . Map.toList

instance (ToAvro a) => ToAvro (HashMap.HashMap String a) where
  toAvro = toAvro . HashMap.fromList . map (first Text.pack) . HashMap.toList

instance (ToAvro a) => ToAvro (Maybe a) where
  toAvro a =
    let sch = extractValues $ options (schemaOf a)
    in case a of
      Nothing -> T.Union sch S.Null (toAvro ())
      Just v  -> T.Union sch (schemaOf v) (toAvro v)

instance (ToAvro a) => ToAvro [a] where
  toAvro = T.Array . V.fromList . (toAvro <$>)

instance (ToAvro a) => ToAvro (V.Vector a) where
  toAvro = T.Array . V.map toAvro

instance (U.Unbox a, ToAvro a) => ToAvro (U.Vector a) where
  toAvro = T.Array . V.map toAvro . U.convert
