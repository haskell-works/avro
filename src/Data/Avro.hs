{-# LANGUAGE FlexibleInstances #-}
-- | Avro encoding and decoding routines.
--
-- This library provides a high level interface for encoding (and decoding)
-- Haskell values in Apache's Avro serialization format.  The goal is to
-- match Aeson's API whenever reasonable, meaning user experience with one
-- effectively translate to the other.
--
-- Avro RPC is not currently supported.
--
-- **Library Structure**
--
-- The library structure includes:
--   * This module, 'Data.Avro', providing a high-level interface via
--     classes of 'FromAvro' and 'ToAvro' for decoding and encoding values.
--   * 'Data.Avro.Type' define the types of Avro data, providing a common
--      (intermediate) representation for any data that is encoded or decoded
--      by Data.Avro.
--   * 'Data.Avro.Encode' and 'Data.Avro.Decode': More
--     efficient conversion capable of avoiding the intermediate representation.
--     Also, the implementation of the en/decoding of the intermediate
--     representation.
--   * 'Data.Avro.Deconflict': translate decoded data from an
--     encoder schema to the (potentially different) decoder's schema.
--   * 'Data.Avro.Schema': Defines the type for Avro schema's and its JSON
--      encoding/decoding.
module Data.Avro
  ( FromAvro(..)
  , ToAvro(..)
  , (.:)
  , Result(..), badValue
  , decode
  , decodeContainer
  , decodeContainerBytes
  , encode
  , encodeContainer
  , encodeContainerWithSync
  ) where

import           Prelude              as P
import qualified Data.Avro.Decode     as D
import           Data.Avro.Deconflict as C
import qualified Data.Avro.Encode     as E
import           Data.Avro.Schema     as S
import           Data.Avro.Types      as T
import qualified Data.Binary.Get      as G
import qualified Data.Binary.Put      as P
import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (toList)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Int
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Lazy       as TL
import           Data.Vector          ()
import           Data.Word

-- |Decode a lazy bytestring using a given Schema.
decode :: FromAvro a => Schema -> ByteString -> Result a
decode sch bytes =
  case D.decodeAvro sch bytes of
    Right val -> fromAvro val
    Left err  -> Error err

-- |Decode a container and de-conflict the writer schema with a given
-- reader-schema.  Exceptions are thrown instead of a 'Result' type to
-- allow this function to be read lazy (to be done in some later version).
decodeContainer :: FromAvro a => Schema -> ByteString -> [[a]]
decodeContainer readerSchema bs =
  case D.decodeContainer bs of
    Right (writerSchema,val) ->
      let err e = error $ "Could not deconflict reader and writer schema." <> e
          dec x =
            case C.deconflict writerSchema readerSchema x of
              Left e   -> err e
              Right v  -> case fromAvro v of
                            Success x -> x
                            Error e   -> error e
      in P.map (P.map dec) val
    Left err -> error err

encode :: ToAvro a => a -> BL.ByteString
encode = E.encodeAvro . toAvro

encodeContainer :: ToAvro a => [[a]] -> IO BL.ByteString
encodeContainer = E.encodeContainer . map (map toAvro)

encodeContainerWithSync :: ToAvro a => (Word64,Word64,Word64,Word64) -> [[a]] -> BL.ByteString
encodeContainerWithSync (a,b,c,d) = E.encodeContainerWithSync s . map (map toAvro)
 where s = P.runPut $ mapM_ P.putWord64le [a,b,c,d]

-- |Like 'decodeContainer' but returns the avro-encoded bytes for each
-- object in the container instead of the Haskell type.
--
-- This is particularly useful when slicing up containers into one or more
-- smaller files.  By extracting the original bytestring it is possible to
-- avoid re-encoding data.
decodeContainerBytes :: ByteString -> [[ByteString]]
decodeContainerBytes bs =
  case D.decodeContainerWith schemaBytes bs of
    Right (writerSchema, val) -> val
    Left e -> error $ "Could not decode container: " <> e
  where
  schemaBytes sch =
    do start <- G.bytesRead
       end   <- G.lookAhead $ do _ <- D.getAvroOf sch
                                 G.bytesRead
       G.getLazyByteString (end-start)

class FromAvro a where
  fromAvro :: Value Type -> Result a

instance FromAvro (Value Type) where
  fromAvro v  = pure v
instance FromAvro Bool where
  fromAvro (T.Boolean b) = pure b
  fromAvro v             = badValue v "Bool"
instance FromAvro B.ByteString where
  fromAvro (T.Bytes b) = pure b
  fromAvro v          = badValue v "ByteString"
instance FromAvro BL.ByteString where
  fromAvro (T.Bytes b) = pure (BL.fromStrict b)
  fromAvro v          = badValue v "Lazy ByteString"
instance FromAvro Int where
  fromAvro (T.Int i) | (fromIntegral i :: Integer) < fromIntegral (maxBound :: Int)
                      = pure (fromIntegral i)
  fromAvro v          = badValue v "Int"
instance FromAvro Int32 where
  fromAvro (T.Int i)  = pure (fromIntegral i)
  fromAvro v          = badValue v "Int32"
instance FromAvro Int64 where
  fromAvro (T.Long i) = pure i
  fromAvro (T.Int i)  = pure (fromIntegral i)
  fromAvro v = badValue v "Int64"

instance FromAvro a => FromAvro (Maybe a) where
  fromAvro (T.Union (S.Null :| [_])  _ T.Null) = pure Nothing
  fromAvro (T.Union (S.Null :| [_]) _ v)       = Just <$> fromAvro v
  fromAvro v = badValue v "Maybe a"

instance FromAvro a => FromAvro [a] where
  fromAvro (T.Array vec) = mapM fromAvro $ toList vec
  fromAvro v = badValue v "[a]"

instance FromAvro Text where
  fromAvro (T.String txt) = pure txt
  fromAvro v = badValue v "Text"

instance FromAvro TL.Text where
  fromAvro (T.String txt) = pure (TL.fromStrict txt)
  fromAvro v = badValue v "LazyText"

instance (FromAvro a) => FromAvro (Map.Map Text a) where
  fromAvro (T.Record mp) = mapM fromAvro $ Map.fromList (HashMap.toList mp)
  fromAvro (T.Map mp)  = mapM fromAvro $ Map.fromList (HashMap.toList mp)
  fromAvro v = badValue v "Map Text a"

instance (FromAvro a) => FromAvro (HashMap.HashMap Text a) where
  fromAvro (T.Record mp) = mapM fromAvro mp
  fromAvro (T.Map mp)    = mapM fromAvro mp
  fromAvro v = badValue v "HashMap Text a"

badValue :: Value Type -> String -> Result a
badValue v t = fail $ "Unexpected value when decoding for '" <> t <> "': " <> show v

(.:) :: FromAvro a => HashMap.HashMap Text (Value Type) -> Text -> Result a
(.:) obj key =
  case HashMap.lookup key obj of
    Nothing -> fail $ "Requested field not available: " <> show key
    Just v  -> fromAvro v

class ToAvro a where
  toAvro :: a -> T.Value Type
instance ToAvro () where
  toAvro a = T.Null
instance ToAvro Int where
  toAvro = T.Long . fromIntegral
instance (ToAvro a) => ToAvro (Map.Map Text a) where
  toAvro = toAvro . HashMap.fromList . Map.toList
instance (ToAvro a) => ToAvro (HashMap.HashMap Text a) where
  toAvro mp = T.Map $ HashMap.map toAvro mp
instance (ToAvro a) => ToAvro (Map.Map TL.Text a) where
  toAvro = toAvro . HashMap.fromList . map (\(k,v) -> (TL.toStrict k,v)) . Map.toList
instance (ToAvro a) => ToAvro (HashMap.HashMap TL.Text a) where
  toAvro mp = toAvro $ HashMap.fromList $ map (\(k,v) -> (TL.toStrict k,v)) $ HashMap.toList mp
instance (ToAvro a) => ToAvro (Map.Map String a) where
  toAvro mp = toAvro $ HashMap.fromList $ map (\(k,v) -> (Text.pack k,v)) $ Map.toList mp
instance (ToAvro a) => ToAvro (HashMap.HashMap String a) where
  toAvro mp = toAvro $ HashMap.fromList $ map (\(k,v) -> (Text.pack k,v)) $ HashMap.toList mp

-- @enumToAvro val@ will generate an Avro encoded value of enum suitable
-- for serialization ('encode').
-- enumToAvro :: (Show a, Enum a, Bounded a, Generic a) => a -> T.Value Type
-- enumToAvro e = T.Enum ty (show e)
--  where
--   ty = S.Enum nm [] Nothing Nothing (map (T.pack . show) [minBound..maxBound])
--   nm = datatypeName g
--   g  = from e -- GHC generics
