{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Data.Avro.Decode.Lazy
  ( decodeAvro
  , decodeContainer
  , decodeContainer'
  , decodeContainerWithSchema
  , decodeContainerWithSchema'

  -- * Bypass decoding
  , decodeRawBlocks

  -- * Lower level interface
  , getContainerValues
  , getContainerValuesWith
  , getContainerValuesBytes
  , getContainerValuesBytes'
  , getAvroOf
  , GetAvro(..)
  , FromLazyAvro(..)
  , (.~:)
  , T.LazyValue(..)
  , badValue
  ) where

import           Control.Monad              (foldM, replicateM, when)
import qualified Data.Aeson                 as A
import qualified Data.Array                 as Array
import           Data.Binary.Get            (Get, runGetOrFail)
import qualified Data.Binary.Get            as G
import           Data.Binary.IEEE754        as IEEE
import           Data.Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either                (isRight)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int
import           Data.List                  (foldl', unfoldr)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Monoid                ((<>))
import qualified Data.Set                   as Set
import           Data.Tagged                (Tagged, untag)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Vector                as V
import           Prelude                    as P

import           Data.Avro.Codec                 (Decompress)
import qualified Data.Avro.Decode.Lazy.LazyValue as T
import           Data.Avro.DecodeRaw
import           Data.Avro.HasAvroSchema         (schema)
import           Data.Avro.Schema                as S
import qualified Data.Avro.Types                 as TypesStrict
import           Data.Avro.Zag

import qualified Data.Avro.Decode.Strict.Internal as DecodeStrict

import Data.Avro.Decode.Get
import Data.Avro.Decode.Lazy.Convert      (toStrictValue)
import Data.Avro.Decode.Lazy.Deconflict   as C
import Data.Avro.Decode.Lazy.FromLazyAvro
import Data.Avro.FromAvro

-- | Decodes the container as a lazy list of values of the requested type.
--
-- The schema for the requested type will be de-conflicted with the schema
-- embedded with the container.
--
-- Errors are reported as a part of the list and the list will stop at first
-- error. This means that the consumer will get all the "good" content from
-- the container until the error is detected, then this error and then the list
-- is finished.
decodeContainer :: forall a. FromLazyAvro a => BL.ByteString -> [Either String a]
decodeContainer bs =
  let vals = either (\err -> [Left err]) concat (decodeContainer' bs)
  in takeWhileInclusive isRight vals

-- | Decodes the container as a lazy list of values of the requested type.
--
-- The schema for the requested type will be de-conflicted with the schema
-- embedded with the container.
--
-- The content of the container is returned as a list of "blocks" of values
-- inside this container, so the notion of blocks in the container is preserved.
-- Since decoding is lazy it should be safe to concat these values into one lazy list.
--
-- The "outer" error represents the error in opening the container itself
-- (including problems like reading schemas embedded into the container.)
--
-- The "inner" errors represent problems in decoding individual values.
--
-- Note that this function will not stop decoding at the first occurance of the "inner"
-- error, and will continue attempting decoding values, so it is possible to
-- get 'Right' after 'Left'. It is up to the user to decide whether it is correct or not to
-- continue after errors (most likely it will not be correct).
--
-- 'decodeContainer' function makes a choice to stop after the first error.
decodeContainer' :: forall a. FromLazyAvro a => BL.ByteString -> Either String [[Either String a]]
decodeContainer' = decodeContainerWithSchema' (untag (schema :: Tagged a Schema))

-- | Same as 'decodeContainer' but uses provided schema as a reader schema for the container
-- instead of the schema obtained from the type of 'a'.
--
-- It is up to the user to make sure that the provided schema is compatible with 'a'
-- and with the container's writer schema.
decodeContainerWithSchema :: FromLazyAvro a => Schema -> BL.ByteString -> [Either String a]
decodeContainerWithSchema s bs =
  either (\err -> [Left err]) concat (decodeContainerWithSchema' s bs)

-- | Same as 'decodeContainer'' but uses provided schema as a reader schema for the container
-- instead of the schema obtained from the type of 'a'.
--
-- It is up to the user to make sure that the provided schema is compatible with 'a'
-- and with the container's writer schema.
decodeContainerWithSchema' :: FromLazyAvro a => Schema -> BL.ByteString -> Either String [[Either String a]]
decodeContainerWithSchema' readerSchema bs = do
  (writerSchema, vals) <- getContainerValues bs
  let writerSchema' = S.expandNamedTypes writerSchema
  let readerSchema' = S.expandNamedTypes readerSchema
  pure $ (fmap . fmap) (convertValue writerSchema' readerSchema') vals
  where
    convertValue w r v = resultToEither $ fromLazyAvro (C.deconflictNoResolve w r v)

-- |Decode bytes into a 'Value' as described by Schema.
decodeAvro :: Schema -> BL.ByteString -> T.LazyValue Type
decodeAvro s = snd . getAvroOf s
{-# INLINABLE decodeAvro #-}

-- | Decodes the container into a list of blocks of raw Avro values.
--
-- The content of the container is returned as a list of "blocks" of values
-- inside this container, so the notion of blocks in the container is preserved.
-- Since decoding is lazy it should be safe to concat these values into one lazy list.
--
-- Each 'LazyValue' can be an `Error' and this function doesn't make any attempts
-- of dealing with them leaving it up to the user.
--
-- The "outer" error represents the error in opening the container itself
-- (including problems like reading schemas embedded into the container.)
getContainerValues :: BL.ByteString -> Either String (Schema, [[T.LazyValue Type]])
getContainerValues = getContainerValuesWith getAvroOf
{-# INLINABLE getContainerValues #-}

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
  case runGetOrFail getAvro bs of
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
    else case runGetOrFail (getRawBlock decompress) bs of
      Left (bs', _, err)             -> Left err
      Right (bs', _, (nrObj, bytes)) ->
        case checkMarker sync bs' of
          Left err   -> Left err
          Right rest -> Right $ Just (nrObj, bytes, rest)
  where
    getRawBlock :: Decompress BL.ByteString -> Get (Int, BL.ByteString)
    getRawBlock decompress = do
      nrObj    <- getLong >>= sFromIntegral
      nrBytes  <- getLong
      compressed <- G.getLazyByteString nrBytes
      bytes <- case decompress compressed G.getRemainingLazyByteString of
        Right x  -> pure x
        Left err -> fail err
      pure (nrObj, bytes)

    checkMarker :: BL.ByteString -> BL.ByteString -> Either String BL.ByteString
    checkMarker sync bs =
      case BL.splitAt nrSyncBytes bs of
        (marker, _) | marker /= sync -> Left "Invalid marker, does not match sync bytes."
        (_, rest)                    -> Right rest

getContainerValuesWith :: (Schema -> BL.ByteString -> (BL.ByteString, T.LazyValue Type))
                 -> BL.ByteString
                 -> Either String (Schema, [[T.LazyValue Type]])
getContainerValuesWith schemaToGet bs =
  case decodeRawBlocks bs of
    Left err            -> Left err
    Right (sch, blocks) -> Right (sch, decodeBlocks (schemaToGet sch) blocks)
  where
    decodeBlocks getValue blocks = decodeBlock getValue <$> blocks
    decodeBlock getValue v = case v of
      Left err -> [T.Error err]
      Right (nObj, bytes) ->
        let (_, vs) = consumeN (fromIntegral nObj) getValue bytes
        in vs

decodeGet :: GetAvro a => (a -> T.LazyValue Type) -> BL.ByteString -> (BL.ByteString, T.LazyValue Type)
decodeGet f bs =
  let res = runGetOrFail (f <$> getAvro) bs
  in either (\(rest,_,s) -> (rest, T.Error s)) (\(rest,_,a) -> (rest, a)) res
{-# INLINE decodeGet #-}

-- | Splits container into a list of individual avro-encoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
getContainerValuesBytes :: BL.ByteString -> Either String (Schema, [Either String BL.ByteString])
getContainerValuesBytes =
  extractContainerValues readBytes
  where
    readBytes sch = do
      start <- G.bytesRead
      end <- G.lookAhead (DecodeStrict.getAvroOf sch >> G.bytesRead)
      G.getLazyByteString (end-start)

-- | Splits container into a list of individual avro-encoded values.
-- This version provides both encoded and decoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
getContainerValuesBytes' :: BL.ByteString -> Either String (Schema, [Either String (TypesStrict.Value S.Type, BL.ByteString)])
getContainerValuesBytes' =
  extractContainerValues readBytes
  where
    readBytes sch = do
      start <- G.bytesRead
      (val, end) <- G.lookAhead (DecodeStrict.getAvroOf sch >>= (\v -> (v, ) <$> G.bytesRead))
      res <- G.getLazyByteString (end-start)
      pure (val, res)

extractContainerValues :: (Schema -> Get a) -> BL.ByteString -> Either String (Schema, [Either String a])
extractContainerValues f bs =
  case decodeRawBlocks bs of
    Left err            -> Left err
    Right (sch, blocks) -> Right (sch, blocks >>= decodeBlock sch)
  where
    decodeBlock _ (Left err)               = undefined
    decodeBlock sch (Right (nrObj, bytes)) = snd $ consumeN (fromIntegral nrObj) (decodeValue sch) bytes

    decodeValue sch bytes =
      case G.runGetOrFail (f sch) bytes of
        Left (bs', _, err)  -> (bs', Left err)
        Right (bs', _, res) -> (bs', Right res)

consumeN :: Int64 -> (a -> (a, b)) -> a -> (a, [b])
consumeN n f a =
  if n == 0
    then (a, [])
    else
      let (a', b) = f a
          (r, bs) = consumeN (n-1) f a'
      in (r, b:bs)
{-# INLINE consumeN #-}

getAvroOf :: Schema -> BL.ByteString -> (BL.ByteString, T.LazyValue Type)
getAvroOf ty0 bs = go ty0 bs
  where
  env = S.buildTypeEnvironment envFail ty0
  envFail t = fail $ "Named type not in schema: " <> show t

  go :: Type -> BL.ByteString -> (BL.ByteString, T.LazyValue Type)
  go ty bs =
    case ty of
      Null    -> (bs, T.Null)
      Boolean -> decodeGet T.Boolean  bs
      Int     -> decodeGet T.Int      bs
      Long    -> decodeGet T.Long     bs
      Float   -> decodeGet T.Float    bs
      Double  -> decodeGet T.Double   bs
      Bytes   -> decodeGet T.Bytes    bs
      String  -> decodeGet T.String   bs
      Array t -> T.Array . V.fromList . mconcat <$> getElements bs (go t)
      Map t   -> T.Map . HashMap.fromList . mconcat <$> getKVPairs bs (go t)
      NamedType tn ->
        case runGetOrFail (env tn) bs of
          Left (bs', _, err) -> (bs', T.Error err)
          Right (bs', _, v)  -> go v bs'

      Record {..} -> do
        let getField bs' Field {..} = (fldName,) <$> go fldType bs'
        let flds = foldl' (\(bs', as) fld -> (:as) <$> getField bs' fld ) (bs, []) fields
        T.Record ty . HashMap.fromList <$> flds

      Enum {..} ->
        case runGetOrFail getLong bs of
          Left (bs', _, err) -> (bs', T.Error err)
          Right (bs', _, i)  ->
            case symbolLookup i of
              Nothing  -> (bs', T.Error ("Unknown value {" <> show i <> "} for enum " <> Text.unpack (typeName ty) ))
              Just sym -> (bs', T.Enum ty (fromIntegral i) sym)

      Union ts ->
        case runGetOrFail getLong bs of
          Left (bs', _, err) -> (bs', T.Error err)
          Right (bs', _, i)  ->
            case ts V.!? (fromIntegral i) of
              Nothing -> (bs', T.Error $ "Decoded Avro tag is outside the expected range for a Union. Tag: " <> show i <> " union of: " <> show (V.map typeName ts))
              Just t  -> T.Union ts t <$> go t bs'

      Fixed {..} ->
        case runGetOrFail (G.getByteString (fromIntegral size)) bs of
          Left (bs', _, err) -> (bs', T.Error err)
          Right (bs', _, v)  -> (bs', T.Fixed ty v)
{-# INLINABLE getAvroOf #-}

getKVPair getElement bs =
  case runGetOrFail getString bs of
    Left (bs'', _, err) -> (bs'', ("", T.Error err))
    Right (bs'', _, v)  -> (v,) <$> getElement bs''
{-# INLINE getKVPair #-}

getKVPairs :: BL.ByteString
           -> (BL.ByteString -> (BL.ByteString, T.LazyValue Type))
           -> (BL.ByteString, [[(Text, T.LazyValue Type)]])
getKVPairs bs getElement =
  case runGetOrFail (abs <$> getLong) bs of
    Left (bs', _, err) -> (bs', [[("", T.Error err)]])
    Right (bs', _, l)  | l == 0 -> (bs', [])
    Right (bs', _, l)  ->
      let (bs'', vs) = consumeN l (getKVPair getElement) bs'
          (rest, vs') = getKVPairs bs'' getElement
      in (rest, vs : vs')
{-# INLINE getKVPairs #-}


getElements :: BL.ByteString
            -> (BL.ByteString -> (BL.ByteString, T.LazyValue Type))
            -> (BL.ByteString, [[T.LazyValue Type]])
getElements bs getElement  =
  case runGetOrFail (abs <$> getLong) bs of
    Left (bs', _, err) -> (bs', [[T.Error err]])
    Right (bs', _, l)  | l == 0 -> (bs', [])
    Right (bs', _, l)  ->
      let (bs'', vs) = consumeN l getElement bs'
          (rest, vs') = getElements bs'' getElement
      in (rest, vs : vs')
{-# INLINE getElements #-}

--
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) =
  x : if p x then takeWhileInclusive p xs else []
{-# INLINE takeWhileInclusive #-}
