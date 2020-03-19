{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Avro.Internal.Container
where

import           Control.Monad                 (when)
import qualified Data.Aeson                    as Aeson
import           Data.Avro.Codec               (Codec (..), Decompress)
import qualified Data.Avro.Codec               as Codec
import           Data.Avro.Encoding.EncodeAvro (toEncoding)
import           Data.Avro.Internal.EncodeRaw  (encodeRaw)
import           Data.Avro.Schema.Schema       (Schema)
import qualified Data.Avro.Schema.Schema       as Schema
import           Data.Binary.Get               (Get)
import qualified Data.Binary.Get               as Get
import           Data.ByteString               (ByteString)
import           Data.ByteString.Builder       (Builder, lazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC
import           Data.Either                   (isRight)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                      (Int32, Int64)
import           Data.List                     (foldl', unfoldr)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import           System.Random.TF.Init         (initTFGen)
import           System.Random.TF.Instances    (randoms)

import qualified Data.Avro.Internal.Get as AGet

data ContainerHeader = ContainerHeader
  { syncBytes       :: BL.ByteString
  , decompress      :: forall a. Decompress a
  , containedSchema :: Schema
  }

nrSyncBytes :: Integral sb => sb
nrSyncBytes = 16
{-# INLINE nrSyncBytes #-}

-- | Generates a new synchronization marker for encoding Avro containers
newSyncBytes :: IO BL.ByteString
newSyncBytes = BL.pack . take nrSyncBytes . randoms <$> initTFGen

getContainerHeader :: Get ContainerHeader
getContainerHeader = do
  magic <- getFixed avroMagicSize
  when (BL.fromStrict magic /= avroMagicBytes)
        (fail "Invalid magic number at start of container.")
  metadata <- getMeta
  sync  <- BL.fromStrict <$> getFixed nrSyncBytes
  codec <- parseCodec (Map.lookup "avro.codec" metadata)
  schema <- case Map.lookup "avro.schema" metadata of
              Nothing -> fail "Invalid container object: no schema."
              Just s  -> case Aeson.eitherDecode' s of
                            Left e  -> fail ("Can not decode container schema: " <> e)
                            Right x -> return x
  return ContainerHeader  { syncBytes = sync
                          , decompress = Codec.codecDecompress codec
                          , containedSchema = schema
                          }
  where avroMagicSize :: Integral a => a
        avroMagicSize = 4

        avroMagicBytes :: BL.ByteString
        avroMagicBytes = BLC.pack "Obj" <> BL.pack [1]

        getFixed :: Int -> Get ByteString
        getFixed = Get.getByteString

        getMeta :: Get (Map.Map Text BL.ByteString)
        getMeta =
          let keyValue = (,) <$> AGet.getString <*> AGet.getBytesLazy
          in Map.fromList <$> AGet.decodeBlocks keyValue

-- | Reads the container as a list of blocks without decoding them into actual values.
--
-- This can be useful for streaming / splitting / merging Avro containers without
-- paying the cost for Avro encoding/decoding.
--
-- Each block is returned as a raw 'ByteString' annotated with the number of Avro values
-- that are contained in this block.
--
-- The "outer" error represents the error in opening the container itself
-- (including problems like reading schemas embedded into the container.)
decodeRawBlocks :: BL.ByteString -> Either String (Schema, [Either String (Int, BL.ByteString)])
decodeRawBlocks bs =
  case Get.runGetOrFail getContainerHeader bs of
    Left (bs', _, err) -> Left err
    Right (bs', _, ContainerHeader {..}) ->
      let blocks = allBlocks syncBytes decompress bs'
      in Right (containedSchema, blocks)
  where
    allBlocks sync decompress bytes =
      flip unfoldr (Just bytes) $ \acc -> case acc of
        Just rest -> next sync decompress rest
        Nothing   -> Nothing

    next syncBytes decompress bytes =
      case getNextBlock syncBytes decompress bytes of
        Right (Just (numObj, block, rest)) -> Just (Right (numObj, block), Just rest)
        Right Nothing                      -> Nothing
        Left err                           -> Just (Left err, Nothing)

getNextBlock :: BL.ByteString
             -> Decompress BL.ByteString
             -> BL.ByteString
             -> Either String (Maybe (Int, BL.ByteString, BL.ByteString))
getNextBlock sync decompress bs =
  if BL.null bs
    then Right Nothing
    else case Get.runGetOrFail (getRawBlock decompress) bs of
      Left (bs', _, err)             -> Left err
      Right (bs', _, (nrObj, bytes)) ->
        case checkMarker sync bs' of
          Left err   -> Left err
          Right rest -> Right $ Just (nrObj, bytes, rest)
  where
    getRawBlock :: Decompress BL.ByteString -> Get (Int, BL.ByteString)
    getRawBlock decompress = do
      nrObj    <- AGet.getLong >>= AGet.sFromIntegral
      nrBytes  <- AGet.getLong
      compressed <- Get.getLazyByteString nrBytes
      bytes <- case decompress compressed Get.getRemainingLazyByteString of
        Right x  -> pure x
        Left err -> fail err
      pure (nrObj, bytes)

    checkMarker :: BL.ByteString -> BL.ByteString -> Either String BL.ByteString
    checkMarker sync bs =
      case BL.splitAt nrSyncBytes bs of
        (marker, _) | marker /= sync -> Left "Invalid marker, does not match sync bytes."
        (_, rest)                    -> Right rest

-- | Splits container into a list of individual avro-encoded values.
-- This version provides both encoded and decoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
extractContainerValuesBytes :: forall a schema.
     (Schema -> Either String schema)
  -> (schema -> Get a)
  -> BL.ByteString
  -> Either String (Schema, [Either String (a, BL.ByteString)])
extractContainerValuesBytes deconflict f =
  extractContainerValues deconflict readBytes
  where
    readBytes sch = do
      start <- Get.bytesRead
      (val, end) <- Get.lookAhead (f sch >>= (\v -> (v, ) <$> Get.bytesRead))
      res <- Get.getLazyByteString (end-start)
      pure (val, res)

extractContainerValues :: forall a schema.
     (Schema -> Either String schema)
  -> (schema -> Get a)
  -> BL.ByteString
  -> Either String (Schema, [Either String a])
extractContainerValues deconflict f bs = do
  (sch, blocks) <- decodeRawBlocks bs
  readSchema <- deconflict sch
  pure (sch, takeWhileInclusive isRight $ blocks >>= decodeBlock readSchema)
  where
    decodeBlock _ (Left err)               = undefined
    decodeBlock sch (Right (nrObj, bytes)) = snd $ consumeN (fromIntegral nrObj) (decodeValue sch) bytes

    decodeValue sch bytes =
      case Get.runGetOrFail (f sch) bytes of
        Left (bs', _, err)  -> (bs', Left err)
        Right (bs', _, res) -> (bs', Right res)

-- | Packs a container from a given list of already encoded Avro values
-- Each bytestring should represent exactly one one value serialised to Avro.
packContainerValues :: Codec -> Schema -> [[BL.ByteString]] -> IO BL.ByteString
packContainerValues codec sch values = do
  sync <- newSyncBytes
  pure $ packContainerValuesWithSync codec sch sync values

-- | Packs a container from a given list of already encoded Avro values
-- Each bytestring should represent exactly one one value serialised to Avro.
packContainerValuesWithSync :: Codec -> Schema -> BL.ByteString -> [[BL.ByteString]] -> BL.ByteString
packContainerValuesWithSync = packContainerValuesWithSync' (\_ a -> lazyByteString a)
{-# INLINABLE packContainerValuesWithSync #-}
-- | Packs a container from a given list of already encoded Avro values
-- Each bytestring should represent exactly one one value serialised to Avro.
packContainerValuesWithSync' ::
     (Schema -> a -> Builder)
  -> Codec
  -> Schema
  -> BL.ByteString
  -> [[a]]
  -> BL.ByteString
packContainerValuesWithSync' encode codec sch syncBytes values =
  toLazyByteString $ containerHeaderWithSync codec sch syncBytes <> foldMap putBlock values
  where
    putBlock ys =
      let nrObj = length ys
          nrBytes = BL.length theBytes
          theBytes = codecCompress codec $ toLazyByteString $ foldMap (encode sch) ys
      in encodeRaw @Int32 (fromIntegral nrObj) <>
         encodeRaw nrBytes <>
         lazyByteString theBytes <>
         lazyByteString syncBytes

-- | Packs a new container from a list of already encoded Avro blocks.
-- Each block is denoted as a pair of a number of objects within that block and the block content.
packContainerBlocks :: Codec -> Schema -> [(Int, BL.ByteString)] -> IO BL.ByteString
packContainerBlocks codec sch blocks = do
  sync <- newSyncBytes
  pure $ packContainerBlocksWithSync codec sch sync blocks

-- | Packs a new container from a list of already encoded Avro blocks.
-- Each block is denoted as a pair of a number of objects within that block and the block content.
packContainerBlocksWithSync :: Codec -> Schema -> BL.ByteString -> [(Int, BL.ByteString)] -> BL.ByteString
packContainerBlocksWithSync codec sch syncBytes blocks =
  toLazyByteString $
    containerHeaderWithSync codec sch syncBytes <>
    foldMap putBlock blocks
  where
    putBlock (nrObj, bytes) =
      let compressed = codecCompress codec bytes in
        encodeRaw @Int32 (fromIntegral nrObj) <>
        encodeRaw (BL.length compressed) <>
        lazyByteString compressed <>
        lazyByteString syncBytes


-- | Creates an Avro container header for a given schema.
containerHeaderWithSync :: Codec -> Schema -> BL.ByteString -> Builder
containerHeaderWithSync codec sch syncBytes =
  lazyByteString avroMagicBytes
    <> toEncoding (Schema.Map Schema.Bytes') headers
    <> lazyByteString syncBytes
  where
    avroMagicBytes :: BL.ByteString
    avroMagicBytes = "Obj" <> BL.pack [1]

    headers :: HashMap Text BL.ByteString
    headers =
      HashMap.fromList
        [
          ("avro.schema", Aeson.encode sch)
        , ("avro.codec", BL.fromStrict (codecName codec))
        ]

-----------------------------------------------------------------

consumeN :: Int64 -> (a -> (a, b)) -> a -> (a, [b])
consumeN n f a =
  if n == 0
    then (a, [])
    else
      let (a', b) = f a
          (r, bs) = consumeN (n-1) f a'
      in (r, b:bs)
{-# INLINE consumeN #-}

----------------------------------------------------------------
parseCodec :: Monad m => Maybe BL.ByteString -> m Codec
parseCodec (Just "null")    = pure Codec.nullCodec
parseCodec (Just "deflate") = pure Codec.deflateCodec
parseCodec (Just x)         = error $ "Unrecognized codec: " <> BLC.unpack x
parseCodec Nothing          = pure Codec.nullCodec

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) =
  x : if p x then takeWhileInclusive p xs else []
{-# INLINE takeWhileInclusive #-}
