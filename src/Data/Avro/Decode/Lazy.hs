{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

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
import Data.Avro.Decode.Lazy.Convert      (fromStrictValue, toStrictValue)
import Data.Avro.Decode.Lazy.FromLazyAvro
import Data.Avro.FromAvro
import Data.Avro.Internal.Container       (ContainerHeader (..), consumeN, decodeRawBlocks, extractContainerValuesBytes, getContainerHeader, nrSyncBytes)
import Data.Avro.Internal.Get
import Data.Avro.Schema.Deconflict

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
  (_, vals) <- getDeconflictedValues bs
  pure $ (fmap . fmap) (resultToEither . fromLazyAvro) vals
  where
    getDeconflictedValues = getContainerValuesWith (getAvroOf' . flip deconflict readerSchema)
    getAvroOf' :: Either String Schema -> BL.ByteString -> (BL.ByteString, T.LazyValue Schema)
    getAvroOf' (Left err) bs = (bs, T.Error err)
    getAvroOf' (Right sc) bs = getAvroOf sc bs

-- |Decode bytes into a 'Value' as described by Schema.
decodeAvro :: Schema -> BL.ByteString -> T.LazyValue Schema
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
getContainerValues :: BL.ByteString -> Either String (Schema, [[T.LazyValue Schema]])
getContainerValues = getContainerValuesWith getAvroOf
{-# INLINABLE getContainerValues #-}

getContainerValuesWith :: (Schema -> BL.ByteString -> (BL.ByteString, T.LazyValue Schema))
                 -> BL.ByteString
                 -> Either String (Schema, [[T.LazyValue Schema]])
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

decodeGet :: GetAvro a => (a -> T.LazyValue Schema) -> BL.ByteString -> (BL.ByteString, T.LazyValue Schema)
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
getContainerValuesBytes = (fmap . fmap . fmap . fmap) snd . extractContainerValuesBytes pure DecodeStrict.getAvroOf
{-# INLINE getContainerValuesBytes #-}

-- | Splits container into a list of individual avro-encoded values.
-- This version provides both encoded and decoded values.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
getContainerValuesBytes' :: BL.ByteString -> Either String (Schema, [Either String (TypesStrict.Value S.Schema, BL.ByteString)])
getContainerValuesBytes' = extractContainerValuesBytes pure DecodeStrict.getAvroOf
{-# INLINE getContainerValuesBytes' #-}

getAvroOf :: Schema -> BL.ByteString -> (BL.ByteString, T.LazyValue Schema)
getAvroOf ty0 bs = go ty0 bs
  where
  env = S.buildTypeEnvironment envFail ty0
  envFail t = fail $ "Named type not in schema: " <> show t

  go :: Schema -> BL.ByteString -> (BL.ByteString, T.LazyValue Schema)
  go ty bs =
    case ty of
      Null     -> (bs, T.Null)
      Boolean  -> decodeGet T.Boolean     bs
      Int _    -> decodeGet (T.Int ty)    bs
      Long _   -> decodeGet (T.Long ty)   bs
      Float    -> decodeGet (T.Float ty)  bs
      Double   -> decodeGet (T.Double ty) bs
      Bytes _  -> decodeGet (T.Bytes ty)  bs
      String _ -> decodeGet (T.String ty) bs
      Array t  -> T.Array . V.fromList . mconcat <$> getElements bs (go t)
      Map t    -> T.Map . HashMap.fromList . mconcat <$> getKVPairs bs (go t)
      NamedType tn ->
        case runGetOrFail (env tn) bs of
          Left (bs', _, err) -> (bs', T.Error err)
          Right (bs', _, v)  -> go v bs'

      Record {..} -> do
        let flds = foldl' (\(bs', as) fld -> (:as) <$> getField fld bs' ) (bs, []) fields
        T.Record ty . HashMap.fromList . catMaybes <$> flds

      Enum {..} ->
        case runGetOrFail getLong bs of
          Left (bs', _, err) -> (bs', T.Error err)
          Right (bs', _, i)  ->
            case symbols V.!? (fromIntegral i) of
              Nothing  -> (bs', T.Error ("Unknown value {" <> show i <> "} for enum " <> Text.unpack (typeName ty) ))
              Just sym -> (bs', T.Enum ty (fromIntegral i) sym)

      Union ts ->
        case runGetOrFail getLong bs of
          Left (bs', _, err) -> (bs', T.Error err)
          Right (bs', _, i)  ->
            case ts `ivElem` (fromIntegral i) of
              Nothing -> (bs', T.Error $ "Decoded Avro tag is outside the expected range for a Union. Tag: " <> show i <> " union of: " <> show ts)
              Just t  -> T.Union (extractValues ts) t <$> go t bs'

      Fixed {..} ->
        case runGetOrFail (G.getByteString (fromIntegral size)) bs of
          Left (bs', _, err) -> (bs', T.Error err)
          Right (bs', _, v)  -> (bs', T.Fixed ty v)

      IntLongCoercion     -> decodeGet @Int32 (T.Long ty   . fromIntegral) bs
      IntFloatCoercion    -> decodeGet @Int32 (T.Float ty  . fromIntegral) bs
      IntDoubleCoercion   -> decodeGet @Int32 (T.Double ty . fromIntegral) bs
      LongFloatCoercion   -> decodeGet @Int64 (T.Float ty  . fromIntegral) bs
      LongDoubleCoercion  -> decodeGet @Int64 (T.Double ty . fromIntegral) bs
      FloatDoubleCoercion -> decodeGet @Float (T.Double ty . realToFrac)   bs
      FreeUnion {..} -> T.Union (V.singleton ty) ty <$> go ty bs

  getField :: Field -> BL.ByteString -> (BL.ByteString, Maybe (Text, T.LazyValue Schema))
  getField Field{..} bs =
    case fldStatus of
      AsIs _        -> Just . (fldName,) <$> go fldType bs
      Defaulted _ v -> (bs, Just (fldName, fromStrictValue v))
      Ignored       -> (fst (go fldType bs), Nothing)
{-# INLINABLE getAvroOf #-}

getKVPair getElement bs =
  case runGetOrFail getString bs of
    Left (bs'', _, err) -> (bs'', ("", T.Error err))
    Right (bs'', _, v)  -> (v,) <$> getElement bs''
{-# INLINE getKVPair #-}

getKVPairs :: BL.ByteString
           -> (BL.ByteString -> (BL.ByteString, T.LazyValue Schema))
           -> (BL.ByteString, [[(Text, T.LazyValue Schema)]])
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
            -> (BL.ByteString -> (BL.ByteString, T.LazyValue Schema))
            -> (BL.ByteString, [[T.LazyValue Schema]])
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
