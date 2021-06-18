{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
module Data.Avro.Encoding.FromAvro
( FromAvro(..)
  -- ** For internal use
, Value(..)
, getValue
)
where

import           Control.DeepSeq             (NFData)
import           Control.Monad               (forM, replicateM)
import           Control.Monad.Identity      (Identity (..))
import           Control.Monad.ST            (ST)
import qualified Data.Aeson                  as A
import qualified Data.Avro.Internal.Get      as Get
import           Data.Avro.Internal.Time
import           Data.Avro.Schema.Decimal    as D
import           Data.Avro.Schema.ReadSchema (ReadSchema)
import qualified Data.Avro.Schema.ReadSchema as ReadSchema
import qualified Data.Avro.Schema.Schema     as Schema
import           Data.Binary.Get             (Get, getByteString, runGetOrFail)
import qualified Data.ByteString             as BS
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Char                   as Char
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Int
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Foldable               (traverse_)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as Text
import qualified Data.Time                   as Time
import qualified Data.UUID                   as UUID
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as UV
import           GHC.Generics                (Generic)
import           GHC.TypeLits

-- | An intermediate data structute for decoding between Avro bytes and Haskell types.
--
-- Because reader and writer schemas, and therefore expected data types and layout
-- can be different, deserialising bytes into Haskell types directly is not possible.
--
-- To overcome this issue this intermediate data structure is used: bytes are decoded into
-- values of type 'Value' (using reader's layout and rules) and then translated to target
-- Haskell types using 'FromAvro' type class machinery.
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
      | Record  ReadSchema (Vector Value)
      | Union   ReadSchema {-# UNPACK #-} Int Value
      | Fixed   ReadSchema {-# UNPACK #-} BS.ByteString
      | Enum    ReadSchema {-# UNPACK #-} Int {-# UNPACK #-} Text
  deriving (Eq, Show, Generic, NFData)

-- | Descrive the value in a way that is safe to use in error messages
-- (i.e. do not print values)
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
  Record s vs   -> "Record (name = " <> show (ReadSchema.name s) <> " fieldsNum = " <> show (V.length vs) <> ")"

--------------------------------------------------------------------------

-- fromRecord :: Schema -> Either String a

-- | Descrives how to convert a given intermediate 'Value' into a Haskell data type.
class FromAvro a where
  fromAvro :: Value -> Either String a

instance FromAvro Int where
  fromAvro (Int _ x)  = Right (fromIntegral x)
  fromAvro (Long _ x) = Right (fromIntegral x)
  fromAvro x          = Left ("Unable to decode Int from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro Int32 where
  fromAvro (Int _ x) = Right x
  fromAvro x         = Left ("Unable to decode Int32 from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro Int64 where
  fromAvro (Long _ x) = Right x
  fromAvro (Int _ x)  = Right (fromIntegral x)
  fromAvro x          = Left ("Unable to decode Int64 from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro Double where
  fromAvro (Double _ x) = Right x
  fromAvro (Float _ x)  = Right (realToFrac x)
  fromAvro (Long _ x)   = Right (fromIntegral x)
  fromAvro (Int _ x)    = Right (fromIntegral x)
  fromAvro x            = Left ("Unable to decode Double from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro Float where
  fromAvro (Float _ x) = Right x
  fromAvro (Long _ x)  = Right (fromIntegral x)
  fromAvro (Int _ x)   = Right (fromIntegral x)
  fromAvro x           = Left ("Unable to decode Double from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro () where
  fromAvro Null = Right ()
  fromAvro x    = Left ("Unable to decode () from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro Bool where
  fromAvro (Boolean x) = Right x
  fromAvro x           = Left ("Unable to decode Bool from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro Text where
  fromAvro (String _ x) = Right x
  fromAvro (Bytes _ x) = case Text.decodeUtf8' x of
    Left unicodeExc -> Left (show unicodeExc)
    Right text      -> Right text
  fromAvro x          = Left ("Unable to decode Text from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro BS.ByteString where
  fromAvro (Bytes _ x)  = Right x
  fromAvro (String _ x) = Right (Text.encodeUtf8 x)
  fromAvro x            = Left ("Unable to decode Bytes from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro BL.ByteString where
  fromAvro (Bytes _ bs) = Right (BL.fromStrict bs)
  fromAvro (String _ x) = Right (BL.fromStrict $ Text.encodeUtf8 x)
  fromAvro x            = Left ("Unable to decode Bytes from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance (KnownNat p, KnownNat s) => FromAvro (D.Decimal p s) where
  fromAvro (Long _ n) = Right $ D.fromUnderlyingValue $ fromIntegral n
  fromAvro (Int _ n)  = Right $ D.fromUnderlyingValue $ fromIntegral n
  fromAvro x          = Left ("Unable to decode Decimal from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro UUID.UUID where
  fromAvro (String _ x) =
    case UUID.fromText x of
      Nothing -> Left "Unable to UUID from a given String value"
      Just u  -> Right u
  fromAvro x            = Left ("Unable to decode UUID from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro Time.Day where
  fromAvro (Int (ReadSchema.Int (Just ReadSchema.Date)) n) = Right $ fromDaysSinceEpoch (toInteger n)
  fromAvro x                                               = Left ("Unable to decode Day from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro Time.DiffTime where
  fromAvro (Int (ReadSchema.Int (Just ReadSchema.TimeMillis)) n)          = Right $ millisToDiffTime (toInteger n)
  fromAvro (Long (ReadSchema.Long _ (Just ReadSchema.TimestampMillis)) n) = Right $ millisToDiffTime (toInteger n)
  fromAvro (Long (ReadSchema.Long _ (Just ReadSchema.TimeMicros)) n)      = Right $ microsToDiffTime (toInteger n)
  fromAvro (Long (ReadSchema.Long _ (Just ReadSchema.TimestampMicros)) n) = Right $ microsToDiffTime (toInteger n)
  fromAvro x                                                              = Left ("Unable to decode TimeDiff from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro Time.UTCTime where
  fromAvro (Long (ReadSchema.Long _ (Just ReadSchema.TimestampMicros)) n) = Right $ microsToUTCTime (toInteger n)
  fromAvro (Long (ReadSchema.Long _ (Just ReadSchema.TimestampMillis)) n) = Right $ millisToUTCTime (toInteger n)
  fromAvro x                                                              = Left ("Unable to decode UTCTime from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro a => FromAvro [a] where
  fromAvro (Array vec) = mapM fromAvro $ V.toList vec
  fromAvro x           = Left ("Unable to decode Array from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro a => FromAvro (Vector a) where
  fromAvro (Array vec) = mapM fromAvro vec
  fromAvro x           = Left ("Unable to decode Array from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance (UV.Unbox a, FromAvro a) => FromAvro (UV.Vector a) where
  fromAvro (Array vec) = UV.convert <$> mapM fromAvro vec
  fromAvro x           = Left ("Unable to decode Array from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro a => FromAvro (Identity a) where
  fromAvro (Union _ 0 v) = Identity <$> fromAvro v
  fromAvro (Union _ n _) = Left ("Unable to decode Identity value from value with a position #" <> show n)
  fromAvro x             = Left ("Unable to decode Identity from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro a => FromAvro (Maybe a) where
  fromAvro (Union _ _ Null) = Right Nothing
  fromAvro (Union _ _ v)    = Just <$> fromAvro v
  fromAvro x                = Left ("Unable to decode Maybe from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance (FromAvro a, FromAvro b) => FromAvro (Either a b) where
  fromAvro (Union _ 0 a) = Left <$> fromAvro a
  fromAvro (Union _ 1 b) = Right <$> fromAvro b
  fromAvro (Union _ n _) = Left ("Unable to decode Either value with a position #" <> show n)
  fromAvro x             = Left ("Unable to decode Either from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro a => FromAvro (Map.Map Text a) where
  fromAvro (Map mp) = traverse fromAvro (Map.fromList (HashMap.toList mp))
  fromAvro x        = Left ("Unable to decode Map from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}

instance FromAvro a => FromAvro (HashMap.HashMap Text a) where
  fromAvro (Map mp) = traverse fromAvro mp
  fromAvro x        = Left ("Unable to decode Map from: " <> show (describeValue x))
  {-# INLINE fromAvro #-}


getValue :: ReadSchema -> Get Value
getValue sch =
  let env = ReadSchema.extractBindings sch
  in getField env sch

getField :: HashMap Schema.TypeName ReadSchema -> ReadSchema -> Get Value
getField env sch = case sch of
  ReadSchema.Null     -> pure Null
  ReadSchema.Boolean  -> fmap Boolean                Get.getBoolean

  ReadSchema.Int _ -> fmap (Int sch)              Get.getInt

  ReadSchema.Long ReadSchema.ReadLong _     -> fmap (Long sch)                Get.getLong
  ReadSchema.Long ReadSchema.LongFromInt _  -> fmap (Long sch . fromIntegral)  Get.getInt

  ReadSchema.Float ReadSchema.ReadFloat      -> fmap (Float sch)                Get.getFloat
  ReadSchema.Float ReadSchema.FloatFromInt   -> fmap (Float sch . fromIntegral) Get.getInt
  ReadSchema.Float ReadSchema.FloatFromLong  -> fmap (Float sch . fromIntegral) Get.getLong

  ReadSchema.Double ReadSchema.ReadDouble      -> fmap (Double sch)                 Get.getDouble
  ReadSchema.Double ReadSchema.DoubleFromInt   -> fmap (Double sch . fromIntegral)  Get.getInt
  ReadSchema.Double ReadSchema.DoubleFromFloat -> fmap (Double sch . realToFrac)    Get.getFloat
  ReadSchema.Double ReadSchema.DoubleFromLong  -> fmap (Double sch . fromIntegral)  Get.getLong

  ReadSchema.String _              -> fmap (String sch)           Get.getString
  ReadSchema.Record _ _ _ fields   -> fmap (Record sch)             (getRecord env fields)
  ReadSchema.Bytes _               -> fmap (Bytes sch)            Get.getBytes

  ReadSchema.NamedType tn          ->
    case HashMap.lookup tn env of
      Nothing -> fail $ "Unable to resolve type name " <> show tn
      Just r  -> getField env r

  ReadSchema.Enum _ _ _ symbs      -> do
    i <- Get.getLong
    case symbs V.!? fromIntegral i of
      Nothing -> fail $ "Enum " <> show symbs <> " doesn't contain value at position " <> show i
      Just v  -> pure $ Enum sch (fromIntegral i) v

  ReadSchema.Union opts            -> do
    i <- Get.getLong
    case opts V.!? fromIntegral i of
      Nothing      -> fail $ "Decoded Avro tag is outside the expected range for a Union. Tag: " <> show i <> " union of: " <> show opts
      Just (i', t) -> Union sch (fromIntegral i') <$> getField env t

  ReadSchema.Fixed _ _ size _ -> Fixed sch <$> getByteString (fromIntegral size)

  ReadSchema.Array t -> do
    vals <- getBlocksOf env t
    pure $ Array (V.fromList $ mconcat vals)

  ReadSchema.Map  t  -> do
    kvs <- getKVBlocks env t
    return $ Map (HashMap.fromList $ mconcat kvs)

  ReadSchema.FreeUnion ix t -> do
    v <- getField env t
    pure $ Union sch ix v

getKVBlocks :: HashMap Schema.TypeName ReadSchema -> ReadSchema -> Get [[(Text, Value)]]
getKVBlocks env t = do
  blockLength <- abs <$> Get.getLong
  if blockLength == 0
  then return []
  else do vs <- replicateM (fromIntegral blockLength) ((,) <$> Get.getString <*> getField env t)
          (vs:) <$> getKVBlocks env t
{-# INLINE getKVBlocks #-}

getBlocksOf :: HashMap Schema.TypeName ReadSchema -> ReadSchema -> Get [[Value]]
getBlocksOf env t = do
  blockLength <- abs <$> Get.getLong
  if blockLength == 0
  then return []
  else do
    vs <- replicateM (fromIntegral blockLength) (getField env t)
    (vs:) <$> getBlocksOf env t

getRecord :: HashMap Schema.TypeName ReadSchema -> [ReadSchema.ReadField] -> Get (Vector Value)
getRecord env fs = do
  moos <- fmap concat . forM fs $ \f ->
    case ReadSchema.fldStatus f of
      ReadSchema.Ignored       -> [] <$ getField env (ReadSchema.fldType f)
      ReadSchema.AsIs i        -> (\f -> [(i,f)]) <$> getField env (ReadSchema.fldType f)
      ReadSchema.Defaulted i v -> pure [(i, convertValue v)] --undefined

  return $ V.create $ do
    vals <- MV.unsafeNew (length moos)
    traverse_ (uncurry (MV.write vals)) moos
    return vals

-- | This function will be unnecessary when we fully migrate to 'Value'
convertValue :: Schema.DefaultValue -> Value
convertValue = \case
  Schema.DNull -> Null
  Schema.DBoolean v       -> Boolean v
  Schema.DInt s v         -> Int (ReadSchema.fromSchema s) v
  Schema.DLong s v        -> Long (ReadSchema.fromSchema s) v
  Schema.DFloat s v       -> Float (ReadSchema.fromSchema s) v
  Schema.DDouble s v      -> Double (ReadSchema.fromSchema s) v
  Schema.DBytes s v       -> Bytes (ReadSchema.fromSchema s) v
  Schema.DString s v      -> String (ReadSchema.fromSchema s) v
  Schema.DArray v         -> Array $ fmap convertValue v
  Schema.DMap v           -> Map $ fmap convertValue v
  Schema.DFixed s v       -> Fixed (ReadSchema.fromSchema s) v
  Schema.DEnum s i v      -> Enum (ReadSchema.fromSchema s) i v
  Schema.DUnion vs sch v  ->
    case V.elemIndex sch vs of
      Just ix -> Union (ReadSchema.fromSchema sch) ix (convertValue v)
      Nothing -> error "Union contains a value of an unknown schema"
  Schema.DRecord sch vs   ->
    let
      fldNames = Schema.fldName <$> Schema.fields sch
      values = fmap (\n -> convertValue $ vs HashMap.! n) fldNames
    in Record (ReadSchema.fromSchema sch) $ V.fromList values
