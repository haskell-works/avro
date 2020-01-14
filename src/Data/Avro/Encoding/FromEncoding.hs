{-# LANGUAGE TupleSections #-}
module Data.Avro.Encoding.FromEncoding
where

import           Control.Monad               (forM, replicateM)
import           Control.Monad.ST            (ST)
import           Data.Avro.Encoding.Convert  (convertValue)
import           Data.Avro.Encoding.Value
import qualified Data.Avro.Internal.Get      as Get
import           Data.Avro.Schema            (TypeName)
import           Data.Avro.Schema.ReadSchema (ReadField, ReadSchema)
import qualified Data.Avro.Schema.ReadSchema as Schema
import           Data.Binary.Get             (Get, getByteString, runGetOrFail)
import qualified Data.ByteString.Lazy        as BL
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Text                   (Text)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV

decodeValueWithSchema :: FromValue a => ReadSchema -> BL.ByteString -> Either String a
decodeValueWithSchema schema = fmap snd . decodeValueWithSchema' schema

decodeValueWithSchema' :: FromValue a => ReadSchema -> BL.ByteString -> Either String (BL.ByteString, a)
decodeValueWithSchema' schema payload =
  case runGetOrFail (getValue schema) payload of
    Right (bs, _, v) -> (bs,) <$> fromValue v
    Left (_, _, e)   -> Left e

getValue :: ReadSchema -> Get Value
getValue sch =
  let env = Schema.extractBindings sch
  in getField env sch

getField :: HashMap TypeName ReadSchema -> ReadSchema -> Get Value
getField env sch = case sch of
  Schema.Null     -> pure Null
  Schema.Boolean  -> fmap Boolean                Get.getBoolean

  Schema.Int _ -> fmap (Int sch)              Get.getInt

  Schema.Long Schema.ReadLong _     -> fmap (Long sch)                Get.getLong
  Schema.Long Schema.LongFromInt _  -> fmap (Long sch . fromIntegral)  Get.getInt

  Schema.Float Schema.ReadFloat      -> fmap (Float sch)                Get.getFloat
  Schema.Float Schema.FloatFromInt   -> fmap (Float sch . fromIntegral) Get.getInt
  Schema.Float Schema.FloatFromLong  -> fmap (Float sch . fromIntegral) Get.getLong

  Schema.Double Schema.ReadDouble      -> fmap (Double sch)                 Get.getDouble
  Schema.Double Schema.DoubleFromInt   -> fmap (Double sch . fromIntegral)  Get.getInt
  Schema.Double Schema.DoubleFromFloat -> fmap (Double sch . realToFrac)    Get.getFloat
  Schema.Double Schema.DoubleFromLong  -> fmap (Double sch . fromIntegral)  Get.getLong

  Schema.String _              -> fmap (String sch)           Get.getString
  Schema.Record _ _ _ _ fields -> fmap Record                 (getRecord env fields)
  Schema.Bytes _               -> fmap (Bytes sch)            Get.getBytes

  Schema.NamedType tn          ->
    case HashMap.lookup tn env of
      Nothing -> fail $ "Unable to resolve type name " <> show tn
      Just r  -> getField env r

  Schema.Enum _ _ _ symbs      -> do
    i <- Get.getLong
    case symbs V.!? fromIntegral i of
      Nothing -> fail $ "Enum " <> show symbs <> " doesn't contain value at position " <> show i
      Just v  -> pure $ Enum sch (fromIntegral i) v

  Schema.Union opts            -> do
    i <- Get.getLong
    case Schema.ivIndexedValue opts (fromIntegral i) of
      Nothing      -> fail $ "Decoded Avro tag is outside the expected range for a Union. Tag: " <> show i <> " union of: " <> show (Schema.extractValues opts)
      Just (i', t) -> Union sch (fromIntegral i') <$> getField env t

  Schema.Fixed _ _ size _ -> Fixed sch <$> getByteString (fromIntegral size)

  Schema.Array t -> do
    vals <- getBlocksOf env t
    pure $ Array (V.fromList $ mconcat vals)

  Schema.Map  t  -> do
    kvs <- getKVBlocks env t
    return $ Map (HashMap.fromList $ mconcat kvs)

  Schema.FreeUnion ix t -> do
    v <- getField env t
    pure $ Union sch ix v

getKVBlocks :: HashMap TypeName ReadSchema -> ReadSchema -> Get [[(Text, Value)]]
getKVBlocks env t = do
  blockLength <- abs <$> Get.getLong
  if blockLength == 0
  then return []
  else do vs <- replicateM (fromIntegral blockLength) ((,) <$> Get.getString <*> getField env t)
          (vs:) <$> getKVBlocks env t
{-# INLINE getKVBlocks #-}

getBlocksOf :: HashMap TypeName ReadSchema -> ReadSchema -> Get [[Value]]
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

getRecord :: HashMap TypeName ReadSchema -> [ReadField] -> Get (Vector Value)
getRecord env fs = do
  moos <- forM fs $ \f ->
    case Schema.fldStatus f of
      Schema.Ignored       -> getField env (Schema.fldType f) >> pure []
      Schema.AsIs i        -> fmap ((:[]) . (i, )) (getField env (Schema.fldType f))
      Schema.Defaulted i v -> pure [(i, convertValue v)] --undefined

  return $ V.create $ do
    vals <- MV.unsafeNew (length fs)
    writeByPositions vals (mconcat moos)
    return vals

