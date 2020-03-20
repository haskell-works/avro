{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
module Data.Avro.Encoding.DecodeAvro where

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
  fromValue (Int (ReadSchema.Int (Just ReadSchema.Date)) n) = Right $ fromDaysSinceEpoch (toInteger n)
  fromValue x                                               = Left ("Unable to decode Day from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Time.DiffTime where
  fromValue (Int (ReadSchema.Int (Just ReadSchema.TimeMillis)) n)          = Right $ millisToDiffTime (toInteger n)
  fromValue (Long (ReadSchema.Long _ (Just ReadSchema.TimestampMillis)) n) = Right $ millisToDiffTime (toInteger n)
  fromValue (Long (ReadSchema.Long _ (Just ReadSchema.TimeMicros)) n)      = Right $ microsToDiffTime (toInteger n)
  fromValue (Long (ReadSchema.Long _ (Just ReadSchema.TimestampMicros)) n) = Right $ microsToDiffTime (toInteger n)
  fromValue x                                                              = Left ("Unable to decode TimeDiff from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro Time.UTCTime where
  fromValue (Long (ReadSchema.Long _ (Just ReadSchema.TimestampMicros)) n) = Right $ microsToUTCTime (toInteger n)
  fromValue (Long (ReadSchema.Long _ (Just ReadSchema.TimestampMillis)) n) = Right $ millisToUTCTime (toInteger n)
  fromValue x                                                              = Left ("Unable to decode UTCTime from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro a => DecodeAvro [a] where
  fromValue (Array vec) = mapM fromValue $ V.toList vec
  fromValue x           = Left ("Unable to decode Array from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance DecodeAvro a => DecodeAvro (Vector a) where
  fromValue (Array vec) = mapM fromValue vec
  fromValue x           = Left ("Unable to decode Array from: " <> show (describeValue x))
  {-# INLINE fromValue #-}

instance (UV.Unbox a, DecodeAvro a) => DecodeAvro (UV.Vector a) where
  fromValue (Array vec) = UV.convert <$> mapM fromValue vec
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
  ReadSchema.Record _ _ _ _ fields -> fmap Record                 (getRecord env fields)
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

writeByPositions :: MV.MVector s Value -> [(Int, Value)] -> ST s ()
writeByPositions mv writes = foldl (>>) (return ()) (fmap (go mv) writes)
  where go :: MV.MVector s Value ->  (Int, Value) -> ST s ()
        go mv (n, v) = MV.write mv n v

getRecord :: HashMap Schema.TypeName ReadSchema -> [ReadSchema.ReadField] -> Get (Vector Value)
getRecord env fs = do
  moos <- forM fs $ \f ->
    case ReadSchema.fldStatus f of
      ReadSchema.Ignored       -> getField env (ReadSchema.fldType f) >> pure []
      ReadSchema.AsIs i        -> fmap ((:[]) . (i, )) (getField env (ReadSchema.fldType f))
      ReadSchema.Defaulted i v -> pure [(i, convertValue v)] --undefined

  return $ V.create $ do
    vals <- MV.unsafeNew (length fs)
    writeByPositions vals (mconcat moos)
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
    in Record $ V.fromList values
