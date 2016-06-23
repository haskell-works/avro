{-# LANGUAGE FlexibleInstances #-}
-- | Avro encoding and decoding routines.
module Data.Avro
  ( FromAvro(..)
  , (.:)
  , Result(..)
  , ToAvro(..)
  , decode
  , decodeContainer
  , decodeContainerBytes
  -- , encode
  -- , encodeContainer
  ) where

import           Prelude as P
import qualified Data.Avro.Decode as D
import           Data.Avro.Deconflict as C
import           Data.Avro.Encode as E
import           Data.Avro.Schema as S
import           Data.Avro.Types  as T
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import           Data.Int
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Vector ()
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

-- |Like 'decodeContainer' but returns the avro-encoded bytes for each
-- object in the container instead of the Haskell type.
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
  fromAvro v             = fail $ "Invalid Avro value for Bool:" <> show v
instance FromAvro B.ByteString where
  fromAvro (T.Bytes b) = pure b
  fromAvro _ = fail "Invalid Avro value for ByteString"
instance FromAvro BL.ByteString where
  fromAvro (T.Bytes b) = pure (BL.fromStrict b)
  fromAvro _ = fail "Invalid Avro value for Lazy ByteString"
instance FromAvro Int where
  fromAvro (T.Int i)  = pure (fromIntegral i)
  fromAvro _          = fail "Invalid Avro value for Int64"
instance FromAvro Int32 where
  fromAvro (T.Int i)  = pure (fromIntegral i)
  fromAvro v          = fail $ "Invalid Avro value for Int64" <> show v
instance FromAvro Int64 where
  fromAvro (T.Long i) = pure i
  fromAvro (T.Int i)  = pure (fromIntegral i)
  fromAvro v          = fail $ "Invalid Avro value for Int64: " <> show v

instance FromAvro a => FromAvro (Maybe a) where
  fromAvro (T.Union _ _ T.Null) = pure Nothing
  fromAvro (T.Union _ _ v) = Just <$> fromAvro v
  fromAvro _ = fail "Invalid Avro value for 'Maybe a'"

instance FromAvro a => FromAvro [a] where
  fromAvro (T.Array vec) = mapM fromAvro $ toList vec

instance FromAvro Text where
  fromAvro (T.String txt) = pure txt
  fromAvro _ = fail "Invalid Avro value for Text."

instance FromAvro TL.Text where
  fromAvro (T.String txt) = pure (TL.fromStrict txt)
  fromAvro _ = fail "Invalid Avro value for lazy Text."

instance (FromAvro a) => FromAvro (Map.Map Text a) where
  fromAvro (T.Record mp) = mapM fromAvro $ Map.fromList (HashMap.toList mp)
  fromAvro (T.Map mp)  = mapM fromAvro $ Map.fromList (HashMap.toList mp)
  fromAvro _ = fail "Invalid Avro value for Map."

instance (FromAvro a) => FromAvro (HashMap.HashMap Text a) where
  fromAvro (T.Record mp) = mapM fromAvro mp
  fromAvro (T.Map mp)    = mapM fromAvro mp
  fromAvro _ = fail "Invalid Avro value for HashMap."

(.:) :: FromAvro a => HashMap.HashMap Text (Value Type) -> Text -> Result a
(.:) obj key =
  case HashMap.lookup key obj of
    Nothing -> fail $ "Requested field not available: " <> show key
    Just v  -> fromAvro v

class ToAvro a where
  toAvro :: a -> T.Value Type
