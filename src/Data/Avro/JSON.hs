{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TupleSections       #-}
-- | Avro supports a JSON representation of Avro objects alongside the
-- Avro binary format. An Avro schema can be used to generate and
-- validate JSON representations of Avro objects.
--
-- The JSON format is the same format as used for default values in
-- schemas except unions are encoded differently. Non-union values are
-- encoded as follows:
--
-- +--------------+----------+----------+
-- |Avro Type     |JSON Type |Example   |
-- +==============+==========+==========+
-- |null          |null      |null      |
-- +--------------+----------+----------+
-- |boolean       |boolean   |true      |
-- +--------------+----------+----------+
-- |int, long     |integer   |1         |
-- +--------------+----------+----------+
-- |float, double |number    |1.1       |
-- +--------------+----------+----------+
-- |bytes         |string    |"\u00FF"  |
-- +--------------+----------+----------+
-- |string        |string    |"foo"     |
-- +--------------+----------+----------+
-- |record        |object    |{"a":1}   |
-- +--------------+----------+----------+
-- |enum          |string    |"FOO"     |
-- +--------------+----------+----------+
-- |array         |array     |[1]       |
-- +--------------+----------+----------+
-- |map           |object    |{"a":1}   |
-- +--------------+----------+----------+
-- |fixed         |string    |"\u00FF"  |
-- +--------------+----------+----------+
--
-- (Table from the Avro 1.8.2 specification:
-- <https://avro.apache.org/docs/1.8.2/spec.html#schema_record>)
--
-- Bytes and fixed are encoded as JSON strings where each byte is
-- translated into the corresponding Unicode codepoint between 0–255,
-- which includes non-printable characters. Note that this encoding
-- happens at the Unicode code-point level, meaning it is independent
-- of text encoding. (JSON is, by definition, encoded in UTF8.)
--
-- Unions are encoded as an object with a single field that specifies
-- the "branch" of the union. If the branch is a primitive type like
-- @"string"@, the name of the primitive type is used:
--
-- @
-- { "string" : "foo" }
-- @
--
-- For named types (record, enum and fixed), the name of the type is
-- used:
--
-- @
-- { "MyRecord" : { ... } }
-- @
module Data.Avro.JSON where

import           Control.Monad                (forM, when)
import           Control.Monad.Identity       (Identity (..))
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Key       as K
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.Array           as Ar
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import qualified Data.Foldable        as Foldable
import           Data.HashMap.Strict  ((!))
import qualified Data.HashMap.Strict  as HashMap
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust)
import           Data.Tagged
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified Data.Text.Lazy       as LazyText
import qualified Data.Time            as Time

import qualified Data.Avro.HasAvroSchema as Schema
import           Data.Avro.EitherN
import           Data.Avro.Encoding.FromAvro       as FromAvro
import           Data.Avro.Internal.Time
import           Data.Avro.Schema.Decimal as D
import           Data.Avro.Schema.Schema  (DefaultValue (..), Schema)
import qualified Data.Avro.Schema.Schema  as Schema
import           Data.Avro.Schema.ReadSchema (ReadSchema)
import qualified Data.Avro.Schema.ReadSchema as ReadSchema
import qualified Data.Vector              as V
import qualified Data.Vector.Unboxed      as U
import           Data.Int
import           Data.Word
import qualified Data.UUID                as UUID
import           GHC.TypeLits

-- deprecate
decodeAvroJSON :: Schema -> Aeson.Value -> Schema.Result DefaultValue
decodeAvroJSON schema json =
  Schema.parseAvroJSON union env schema json
  where
    env =
      Schema.buildTypeEnvironment missing schema
    missing name =
      fail ("Type " <> show name <> " not in schema")

    union (Schema.Union schemas) Aeson.Null
      | Schema.Null `elem` schemas =
          pure $ Schema.DUnion schemas Schema.Null Schema.DNull
      | otherwise                  =
          fail "Null not in union."
    union (Schema.Union schemas) (Aeson.Object obj)
      | null obj =
          fail "Invalid encoding of union: empty object ({})."
      | length obj > 1 =
          fail "Invalid encoding of union: object with too many fields."
      | otherwise      =
          let
            canonicalize name
              | isBuiltIn name = name
              | otherwise      = Schema.renderFullname $ Schema.parseFullname name
            branch =
              K.toText $ head (KM.keys obj)
            names =
              HashMap.fromList [(Schema.typeName t, t) | t <- Foldable.toList schemas]
          in case HashMap.lookup (canonicalize branch) names of
            Just t  -> do
              nested <- Schema.parseAvroJSON union env t $ case KM.lookup (K.fromText branch) obj of
                Just val -> val
                Nothing -> error "impossible"
              return (Schema.DUnion schemas t nested)
            Nothing -> fail ("Type '" <> Text.unpack branch <> "' not in union: " <> show schemas)
    union Schema.Union{} _ =
      Schema.Error "Invalid JSON representation for union: has to be a JSON object with exactly one field."
    union _ _ =
      error "Impossible: function given non-union schema."

    isBuiltIn name = name `elem` [ "null", "boolean", "int", "long", "float"
                                 , "double", "bytes", "string", "array", "map" ]

class ToAvroJSON a where
  -- | Convert an object with an Avro schema to JSON using that schema.
  --
  -- We always need the schema to /encode/ to JSON because representing
  -- unions requires using the names of named types.
  toAvroJSON :: Schema -> a -> Aeson.Value
  toAvroEncoding :: Schema -> a -> Aeson.Encoding
  toAvroEncoding s = Aeson.toEncoding . toAvroJSON s

instance ToAvroJSON Int where
  -- It might seem surprising that we use fromIntegral here, but it's
  -- used to ensure that overflow is handled correctly.
  toAvroJSON (Schema.Long _) i = Aeson.toJSON @Int64 (fromIntegral i)
  toAvroJSON (Schema.Int _) i  = Aeson.toJSON @Int32 (fromIntegral i)
  toAvroJSON s _               = error ("Unable to encode Int as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON Int32 where
  toAvroJSON (Schema.Long _) i = Aeson.toJSON @Int64 (fromIntegral i)
  toAvroJSON (Schema.Int _) i  = Aeson.toJSON @Int32 i
  toAvroJSON Schema.Double i   = toAvroJSON @Double Schema.Double (fromIntegral i)
  toAvroJSON Schema.Float i    = toAvroJSON @Float Schema.Float (fromIntegral i)
  toAvroJSON s _               = error ("Unable to encode Int32 as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON Int64 where
  toAvroJSON (Schema.Long _) i = Aeson.toJSON i
  toAvroJSON Schema.Double i   = toAvroJSON @Double Schema.Double (fromIntegral i)
  toAvroJSON Schema.Float i    = toAvroJSON @Float Schema.Float (fromIntegral i)
  toAvroJSON s _               = error ("Unable to encode Int64 as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON Word8 where
  toAvroJSON (Schema.Int _) i  = Aeson.toJSON @Word8 i
  toAvroJSON (Schema.Long _) i = Aeson.toJSON @Word64 (fromIntegral i)
  toAvroJSON Schema.Double i   = toAvroJSON @Double Schema.Double (fromIntegral i)
  toAvroJSON Schema.Float i    = toAvroJSON @Float Schema.Float (fromIntegral i)
  toAvroJSON s _               = error ("Unable to encode Word8 as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON Word16 where
  toAvroJSON (Schema.Int _) i  = Aeson.toJSON @Word16 i
  toAvroJSON (Schema.Long _) i = Aeson.toJSON @Word64 (fromIntegral i)
  toAvroJSON Schema.Double i   = toAvroJSON @Double Schema.Double (fromIntegral i)
  toAvroJSON Schema.Float i    = toAvroJSON @Float Schema.Float (fromIntegral i)
  toAvroJSON s _               = error ("Unable to encode Word16 as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON Word32 where
  toAvroJSON (Schema.Int _) i  = Aeson.toJSON @Word32 i
  toAvroJSON (Schema.Long _) i = Aeson.toJSON @Word64 (fromIntegral i)
  toAvroJSON Schema.Double i   = toAvroJSON @Double Schema.Double (fromIntegral i)
  toAvroJSON Schema.Float i    = toAvroJSON @Float Schema.Float (fromIntegral i)
  toAvroJSON s _               = error ("Unable to encode Word32 as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON Word64 where
  toAvroJSON (Schema.Long _) i = Aeson.toJSON @Word64 i
  toAvroJSON Schema.Double i   = toAvroJSON @Double Schema.Double (fromIntegral i)
  toAvroJSON s _               = error ("Unable to encode Word64 as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON Double where
  toAvroJSON Schema.Double i = Aeson.toJSON @Double i
  toAvroJSON s _             = error ("Unable to encode Double as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON Float where
  toAvroJSON Schema.Float i  = Aeson.toJSON @Float i
  toAvroJSON Schema.Double i = Aeson.toJSON @Double $ realToFrac i
  toAvroJSON s _             = error ("Unable to encode Float as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON () where
  toAvroJSON Schema.Null () = Aeson.Null
  toAvroJSON s ()           = error ("Unable to encode () as: " <> show s)
  {-# INLINE toAvroJSON #-}


instance ToAvroJSON Bool where
  toAvroJSON Schema.Boolean v = Aeson.toJSON v
  toAvroJSON s _              = error ("Unable to encode Bool as: " <> show s)
  {-# INLINE toAvroJSON #-}
  toAvroEncoding Schema.Boolean v = Aeson.toEncoding v
  toAvroEncoding s _              = error ("Unable to encode Bool as: " <> show s)
  {-# INLINE toAvroEncoding #-}

instance ToAvroJSON Text.Text where
  toAvroJSON (Schema.Bytes _)  v = Aeson.toJSON $ Schema.serializeBytes $ Text.encodeUtf8 v
  toAvroJSON (Schema.String _) v = Aeson.toJSON v
  toAvroJSON s _                 = error ("Unable to encode Text as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON LazyText.Text where
  toAvroJSON (Schema.Bytes _)  v = Aeson.toJSON $ Schema.serializeBytes $ Text.encodeUtf8 $ LazyText.toStrict v
  toAvroJSON (Schema.String _) v = Aeson.toJSON v
  toAvroJSON s _                 = error ("Unable to encode Text as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON BS.ByteString where
  toAvroJSON s bs = case s of
    (Schema.Bytes _)                         -> Aeson.toJSON $ Schema.serializeBytes bs
    (Schema.String _)                        -> Aeson.toJSON $ Text.decodeUtf8 bs
    Schema.Fixed _ _ l _ | l == BS.length bs -> Aeson.toJSON $ Schema.serializeBytes bs
    Schema.Fixed _ _ l _                     -> error ("Unable to encode ByteString as Fixed(" <> show l <> ") because its length is " <> show (BS.length bs))
    _                                        -> error ("Unable to encode ByteString as: " <> show s)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON BL.ByteString where
  toAvroJSON s bs = toAvroJSON s (BL.toStrict bs)
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON Time.UTCTime where
  toAvroJSON s@(Schema.Long (Just Schema.TimestampMicros)) = toAvroJSON @Int64 s . fromIntegral . utcTimeToMicros
  toAvroJSON s@(Schema.Long (Just Schema.TimestampMillis)) = toAvroJSON @Int64 s . fromIntegral . utcTimeToMillis
  toAvroJSON s                                             = error ("Unable to encode UTCTime as " <> show s)

instance ToAvroJSON Time.LocalTime where
  toAvroJSON s@(Schema.Long (Just Schema.LocalTimestampMicros)) =
    toAvroJSON @Int64 s . fromIntegral . localTimeToMicros
  toAvroJSON s@(Schema.Long (Just Schema.LocalTimestampMillis)) =
    toAvroJSON @Int64 s . fromIntegral . localTimeToMillis
  toAvroJSON s =
    error ("Unable to encode LocalTime as " <> show s)

instance ToAvroJSON Time.DiffTime where
  toAvroJSON s@(Schema.Long (Just Schema.TimeMicros))      = toAvroJSON @Int64 s . fromIntegral . diffTimeToMicros
  toAvroJSON s@(Schema.Long (Just Schema.TimestampMicros)) = toAvroJSON @Int64 s . fromIntegral . diffTimeToMicros
  toAvroJSON s@(Schema.Long (Just Schema.TimestampMillis)) = toAvroJSON @Int64 s . fromIntegral . diffTimeToMillis
  toAvroJSON s@(Schema.Int  (Just Schema.TimeMillis))      = toAvroJSON @Int32 s . fromIntegral . diffTimeToMillis
  toAvroJSON s                                   = error ("Unble to encode DiffTime as " <> show s)

instance ToAvroJSON Time.Day where
  toAvroJSON s = toAvroJSON @Int32 s . fromIntegral . daysSinceEpoch
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON UUID.UUID where
  toAvroJSON s = toAvroJSON s . UUID.toText
  {-# INLINE toAvroJSON #-}

instance ToAvroJSON a => ToAvroJSON [a] where
  toAvroJSON (Schema.Array s) as = Aeson.toJSON $ fmap (toAvroJSON s) as
  toAvroJSON s _                 = error ("Unable to encode Haskell list as: " <> show s)

instance (ToAvroJSON a) => ToAvroJSON (Identity a) where
  toAvroJSON (Schema.Union opts) e@(Identity a) =
    if V.length opts == 1
      then toAvroUnionJSON (V.unsafeIndex opts 0) a
      else error ("Unable to encode Identity as a single-value union: " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Identity value as " <> show s)

instance ToAvroJSON a => ToAvroJSON (Maybe a) where
  toAvroJSON (Schema.Union opts) v =
    case Foldable.toList opts of
      [Schema.Null, s] -> maybe (toAvroJSON Schema.Null ()) (toAvroUnionJSON s) v
      [s, Schema.Null] -> maybe (toAvroJSON Schema.Null ()) (toAvroUnionJSON s) v
      wrongOpts   -> error ("Unable to encode Maybe as " <> show wrongOpts)
  toAvroJSON s _ = error ("Unable to encode Maybe as " <> show s)

instance (U.Unbox a, ToAvroJSON a) => ToAvroJSON (U.Vector a) where
  toAvroJSON (Schema.Array s) as = Aeson.toJSON @(V.Vector Aeson.Value) $ fmap (toAvroJSON s) $ U.convert as
  toAvroJSON s _                 = error ("Unable to encode Vector list as: " <> show s)

instance (ToAvroJSON a) => ToAvroJSON (V.Vector a) where
  toAvroJSON (Schema.Array s) as = Aeson.toJSON $ fmap (toAvroJSON s) as
  toAvroJSON s _                 = error ("Unable to encode Vector list as: " <> show s)

instance (ToAvroJSON a, ToAvroJSON b) => ToAvroJSON (Either a b) where
  toAvroJSON (Schema.Union opts) v =
    if V.length opts == 2
      then case v of
        Left a  -> toAvroUnionJSON (V.unsafeIndex opts 0) a
        Right b -> toAvroUnionJSON (V.unsafeIndex opts 1) b
      else error ("Unable to encode Either as " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Either as " <> show s)

instance ToAvroJSON a => ToAvroJSON (Map.Map Text.Text a) where
  toAvroJSON (Schema.Map s) m = Aeson.toJSON $ fmap (toAvroJSON s) m
  toAvroJSON s _                   = error ("Unable to encode Map as: " <> show s)

instance ToAvroJSON a => ToAvroJSON (HashMap.HashMap Text.Text a) where
  toAvroJSON (Schema.Map s) hm = Aeson.toJSON $ fmap (toAvroJSON s) hm
  toAvroJSON s _               = error ("Unable to encode HashMap as: " <> show s)

instance (Ar.Ix i, ToAvroJSON a) => ToAvroJSON (Ar.Array i a) where
  toAvroJSON (Schema.Array s) a = Aeson.toJSON $ fmap (toAvroJSON s) $ Foldable.toList a
  toAvroJSON s _                = error ("Unable to encode indexed Array list as: " <> show s)

-- TODO, I don't know if this is the right way to do this.
instance (KnownNat p, KnownNat s) => ToAvroJSON (Decimal p s) where
  toAvroJSON s = toAvroJSON @Int64 s . fromIntegral . fromJust . D.underlyingValue

instance (ToAvroJSON a, ToAvroJSON b, ToAvroJSON c) => ToAvroJSON (Either3 a b c) where
  toAvroJSON (Schema.Union opts) v =
    if V.length opts == 3
      then case v of
        E3_1 x -> toAvroUnionJSON (V.unsafeIndex opts 0) x
        E3_2 x -> toAvroUnionJSON (V.unsafeIndex opts 1) x
        E3_3 x -> toAvroUnionJSON (V.unsafeIndex opts 2) x
      else error ("Unable to encode Either3 as " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Either3 as " <> show s)

instance (ToAvroJSON a, ToAvroJSON b, ToAvroJSON c, ToAvroJSON d) => ToAvroJSON (Either4 a b c d) where
  toAvroJSON (Schema.Union opts) v =
    if V.length opts == 4
      then case v of
        E4_1 x -> toAvroUnionJSON (V.unsafeIndex opts 0) x
        E4_2 x -> toAvroUnionJSON (V.unsafeIndex opts 1) x
        E4_3 x -> toAvroUnionJSON (V.unsafeIndex opts 2) x
        E4_4 x -> toAvroUnionJSON (V.unsafeIndex opts 3) x
      else error ("Unable to encode Either4 as " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Either4 as " <> show s)

instance (ToAvroJSON a, ToAvroJSON b, ToAvroJSON c, ToAvroJSON d, ToAvroJSON e) => ToAvroJSON (Either5 a b c d e) where
  toAvroJSON (Schema.Union opts) v =
    if V.length opts == 5
      then case v of
        E5_1 x -> toAvroUnionJSON (V.unsafeIndex opts 0) x
        E5_2 x -> toAvroUnionJSON (V.unsafeIndex opts 1) x
        E5_3 x -> toAvroUnionJSON (V.unsafeIndex opts 2) x
        E5_4 x -> toAvroUnionJSON (V.unsafeIndex opts 3) x
        E5_5 x -> toAvroUnionJSON (V.unsafeIndex opts 4) x
      else error ("Unable to encode Either5 as " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Either5 as " <> show s)

instance (ToAvroJSON a, ToAvroJSON b, ToAvroJSON c, ToAvroJSON d, ToAvroJSON e, ToAvroJSON f) => ToAvroJSON (Either6 a b c d e f) where
  toAvroJSON (Schema.Union opts) v =
    if V.length opts == 6
      then case v of
        E6_1 x -> toAvroUnionJSON (V.unsafeIndex opts 0) x
        E6_2 x -> toAvroUnionJSON (V.unsafeIndex opts 1) x
        E6_3 x -> toAvroUnionJSON (V.unsafeIndex opts 2) x
        E6_4 x -> toAvroUnionJSON (V.unsafeIndex opts 3) x
        E6_5 x -> toAvroUnionJSON (V.unsafeIndex opts 4) x
        E6_6 x -> toAvroUnionJSON (V.unsafeIndex opts 5) x
      else error ("Unable to encode Either6 as " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Either6 as " <> show s)

instance (ToAvroJSON a, ToAvroJSON b, ToAvroJSON c, ToAvroJSON d, ToAvroJSON e, ToAvroJSON f, ToAvroJSON g) => ToAvroJSON (Either7 a b c d e f g) where
  toAvroJSON (Schema.Union opts) v =
    if V.length opts == 7
      then case v of
        E7_1 x -> toAvroUnionJSON (V.unsafeIndex opts 0) x
        E7_2 x -> toAvroUnionJSON (V.unsafeIndex opts 1) x
        E7_3 x -> toAvroUnionJSON (V.unsafeIndex opts 2) x
        E7_4 x -> toAvroUnionJSON (V.unsafeIndex opts 3) x
        E7_5 x -> toAvroUnionJSON (V.unsafeIndex opts 4) x
        E7_6 x -> toAvroUnionJSON (V.unsafeIndex opts 5) x
        E7_7 x -> toAvroUnionJSON (V.unsafeIndex opts 6) x
      else error ("Unable to encode Either7 as " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Either7 as " <> show s)

instance (ToAvroJSON a, ToAvroJSON b, ToAvroJSON c, ToAvroJSON d, ToAvroJSON e, ToAvroJSON f, ToAvroJSON g, ToAvroJSON h) => ToAvroJSON (Either8 a b c d e f g h) where
  toAvroJSON (Schema.Union opts) v =
    if V.length opts == 8
      then case v of
        E8_1 x -> toAvroUnionJSON (V.unsafeIndex opts 0) x
        E8_2 x -> toAvroUnionJSON (V.unsafeIndex opts 1) x
        E8_3 x -> toAvroUnionJSON (V.unsafeIndex opts 2) x
        E8_4 x -> toAvroUnionJSON (V.unsafeIndex opts 3) x
        E8_5 x -> toAvroUnionJSON (V.unsafeIndex opts 4) x
        E8_6 x -> toAvroUnionJSON (V.unsafeIndex opts 5) x
        E8_7 x -> toAvroUnionJSON (V.unsafeIndex opts 6) x
        E8_8 x -> toAvroUnionJSON (V.unsafeIndex opts 7) x
      else error ("Unable to encode Either8 as " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Either8 as " <> show s)

instance (ToAvroJSON a, ToAvroJSON b, ToAvroJSON c, ToAvroJSON d, ToAvroJSON e, ToAvroJSON f, ToAvroJSON g, ToAvroJSON h, ToAvroJSON i) => ToAvroJSON (Either9 a b c d e f g h i) where
  toAvroJSON (Schema.Union opts) v =
    if V.length opts == 9
      then case v of
        E9_1 x -> toAvroUnionJSON (V.unsafeIndex opts 0) x
        E9_2 x -> toAvroUnionJSON (V.unsafeIndex opts 1) x
        E9_3 x -> toAvroUnionJSON (V.unsafeIndex opts 2) x
        E9_4 x -> toAvroUnionJSON (V.unsafeIndex opts 3) x
        E9_5 x -> toAvroUnionJSON (V.unsafeIndex opts 4) x
        E9_6 x -> toAvroUnionJSON (V.unsafeIndex opts 5) x
        E9_7 x -> toAvroUnionJSON (V.unsafeIndex opts 6) x
        E9_8 x -> toAvroUnionJSON (V.unsafeIndex opts 7) x
        E9_9 x -> toAvroUnionJSON (V.unsafeIndex opts 8) x
      else error ("Unable to encode Either9 as " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Either9 as " <> show s)

instance (ToAvroJSON a, ToAvroJSON b, ToAvroJSON c, ToAvroJSON d, ToAvroJSON e, ToAvroJSON f, ToAvroJSON g, ToAvroJSON h, ToAvroJSON i, ToAvroJSON j) => ToAvroJSON (Either10 a b c d e f g h i j) where
  toAvroJSON (Schema.Union opts) v =
    if V.length opts == 10
      then case v of
        E10_1 x -> toAvroUnionJSON (V.unsafeIndex opts 0) x
        E10_2 x -> toAvroUnionJSON (V.unsafeIndex opts 1) x
        E10_3 x -> toAvroUnionJSON (V.unsafeIndex opts 2) x
        E10_4 x -> toAvroUnionJSON (V.unsafeIndex opts 3) x
        E10_5 x -> toAvroUnionJSON (V.unsafeIndex opts 4) x
        E10_6 x -> toAvroUnionJSON (V.unsafeIndex opts 5) x
        E10_7 x -> toAvroUnionJSON (V.unsafeIndex opts 6) x
        E10_8 x -> toAvroUnionJSON (V.unsafeIndex opts 7) x
        E10_9 x -> toAvroUnionJSON (V.unsafeIndex opts 8) x
        E10_10 x -> toAvroUnionJSON (V.unsafeIndex opts 9) x
      else error ("Unable to encode Either10 as " <> show opts)
  toAvroJSON s _ = error ("Unable to encode Either10 as " <> show s)

-- | Unions in Avro JSON serialization are encoded as a map with a single key
-- that is the name of the type. This function can be used to serialize union
-- values properly when writing instances of 'ToAvroJSON' for union fields.
toAvroUnionJSON :: ToAvroJSON a => Schema -> a -> Aeson.Value
toAvroUnionJSON s x = case s of
  Schema.Null -> toAvroJSON s x
  -- TODO, not sure if this is supposed to use logical types, need to
  -- compare with other language implementations
  _ -> Aeson.toJSON $ HashMap.singleton (Schema.typeName s) (toAvroJSON s x)

toJSON :: forall a. (Schema.HasAvroSchema a, ToAvroJSON a) => a -> Aeson.Value
toJSON = toAvroJSON schema
  where
    schema = untag (Schema.schema :: Tagged a Schema)

toEncoding :: forall a. (Schema.HasAvroSchema a, ToAvroJSON a) => a -> Aeson.Encoding
toEncoding = toAvroEncoding schema
  where
    schema = untag (Schema.schema :: Tagged a Schema)

fromJSON :: (FromAvro a) => ReadSchema -> Aeson.Value -> Either String a
fromJSON schema json = do
  intermediateValue <- parseAvroJSON union env schema json
  FromAvro.fromAvro intermediateValue
  where
    env =
      ReadSchema.buildTypeEnvironment missing schema
    missing name =
      fail ("Type " <> show name <> " not in schema")

    union :: ReadSchema -> Aeson.Value -> Either String FromAvro.Value
    union (ReadSchema.Union schemas) Aeson.Null =
      case V.find ((== ReadSchema.Null) . snd) schemas of
        Nothing -> Left "Null not in union."
        Just (ix, _) -> pure $ FromAvro.Union (ReadSchema.Union schemas) ix FromAvro.Null
          
    union (ReadSchema.Union schemas) (Aeson.Object obj)
      | null obj =
          Left "Invalid encoding of union: empty object ({})."
      | length obj > 1 =
          Left "Invalid encoding of union: object with too many fields."
      | otherwise      =
          let
            canonicalize name
              | isBuiltIn name = name
              | otherwise      = Schema.renderFullname $ Schema.parseFullname name
            branch =
              K.toText $ head (KM.keys obj)
            names =
              HashMap.fromList [(ReadSchema.typeName t, ixed) | ixed@(_, t) <- Foldable.toList schemas]
          in case HashMap.lookup (canonicalize branch) names of
            Just (ix, t) -> do
              nested <- parseAvroJSON union env t $ case KM.lookup (K.fromText branch) obj of
                Just val -> val
                Nothing -> error "impossible"
              return (FromAvro.Union (ReadSchema.Union schemas) ix nested)
            Nothing -> Left ("Type '" <> Text.unpack branch <> "' not in union: " <> show schemas)
    union ReadSchema.Union{} _ =
      Left "Invalid JSON representation for union: has to be a JSON object with exactly one field."
    -- TODO, not sure what to do with val here
    union (ReadSchema.FreeUnion ix ty) val =  do
      nested <- parseAvroJSON union env ty val
      pure $ FromAvro.Union (ReadSchema.FreeUnion ix ty) ix nested
    union _ _ =
      error "Impossible: function given non-union schema."

    isBuiltIn name = name `elem` [ "null", "boolean", "int", "long", "float"
                                 , "double", "bytes", "string", "array", "map" ]
-- -- | Parse a 'ByteString' as JSON and convert it to a type with an
-- -- Avro schema. Will return 'Error' if the input is not valid JSON or
-- -- the JSON does not convert with the specified schema.
-- parseJSON :: forall a. (FromAvro a) => ByteString -> Result a
-- parseJSON input = case Aeson.eitherDecode input of
--   Left msg    -> Error msg
--   Right value -> fromJSON value
-- | Parse JSON-encoded avro data.
parseAvroJSON :: (ReadSchema -> Aeson.Value -> Either String FromAvro.Value)
                 -- ^ How to handle unions. The way unions are
                 -- formatted in JSON depends on whether we're parsing
                 -- a normal Avro object or we're parsing a default
                 -- declaration in a schema.
                 --
                 -- This function will only ever be passed 'Union'
                 -- schemas. It /should/ error out if this is not the
                 -- case—it represents a bug in this code.
              -> (Schema.TypeName -> Maybe ReadSchema)
              -> ReadSchema
              -> Aeson.Value
              -> Either String FromAvro.Value
parseAvroJSON union env (ReadSchema.NamedType name) av =
  case env name of
    Nothing -> Left $ "Could not resolve type name for " <> Text.unpack (Schema.renderFullname name)
    Just t  -> parseAvroJSON union env t av
parseAvroJSON union _ u@ReadSchema.Union{} av = u `union` av
parseAvroJSON union env ty av =
  case av of
    Aeson.String s ->
      case ty of
        ReadSchema.String _    -> return $ FromAvro.String ty s
        ReadSchema.Enum {..}   ->
            case s `V.elemIndex` symbols of
              Just i  -> pure $ FromAvro.Enum ty i s
              Nothing -> Left $ "JSON string is not one of the expected symbols for enum '" <> show name <> "': " <> Text.unpack s
        ReadSchema.Bytes _     -> FromAvro.Bytes ty <$> (Schema.resultToEither $ Schema.parseBytes s)
        ReadSchema.Fixed {..}  -> do
          bytes <- Schema.resultToEither $ Schema.parseBytes s
          let len = BS.length bytes
          when (len /= size) $
            Left $ "Fixed string wrong size. Expected " <> show size <> " but got " <> show len
          return $ FromAvro.Fixed ty bytes
        _ -> Left $ "Expected type String, Enum, Bytes, or Fixed, but found (Type,Value)="
                <> show (ty, av)
    Aeson.Bool b   -> 
      case ty of
        ReadSchema.Boolean -> return $ FromAvro.Boolean b
        _                  -> avroTypeMismatch ty "boolean"
    Aeson.Number i ->
      case ty of
        ReadSchema.Int _    -> return $ FromAvro.Int    ty (floor i)
        ReadSchema.Long _ _ -> return $ FromAvro.Long   ty (floor i)
        ReadSchema.Float _  -> return $ FromAvro.Float  ty (realToFrac i)
        ReadSchema.Double _ -> return $ FromAvro.Double ty (realToFrac i)
        _      -> avroTypeMismatch ty "number"
    Aeson.Array vec ->
      case ty of
        ReadSchema.Array t -> FromAvro.Array <$> V.mapM (parseAvroJSON union env t) vec
        _       -> avroTypeMismatch ty "array"
    Aeson.Object obj ->
      case ty of
        ReadSchema.Map mTy     -> FromAvro.Map <$> mapM (parseAvroJSON union env mTy) (KM.toHashMapText obj)
        ReadSchema.Record {..} -> do 
          values <- forM (filter ((/= ReadSchema.Ignored) . ReadSchema.fldStatus) fields) $ \ReadSchema.ReadField{..} -> do
            case KM.lookup (K.fromText fldName) obj of
              Nothing ->
                case fldDefault of
                  Just v  -> pure $ FromAvro.convertValue v
                  Nothing -> Left $ "Decode failure: No record field '" <> Text.unpack fldName <> "' and no default in schema."
              Just v  -> parseAvroJSON union env fldType v
          pure $ FromAvro.Record ty $ V.fromList values
        _ -> avroTypeMismatch ty "object"
    Aeson.Null -> case ty of
      ReadSchema.Null -> return FromAvro.Null
      _    -> avroTypeMismatch ty "null"

avroTypeMismatch :: ReadSchema -> Text.Text -> Either String a
avroTypeMismatch expected actual =
  Left $ "Could not resolve type '" <> Text.unpack actual <> "' with expected type: " <> show expected
