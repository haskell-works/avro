{-# LANGUAGE FlexibleInstances #-}
-- | Avro encoding and decoding routines.
module Data.Avro
  ( FromAvro(..)
  , (.:)
  , Result(..)
  , ToAvro(..)
  , decode
  , decodeContainer
  -- , encode
  -- , encodeContainer
  ) where

import Prelude as P
import Data.Avro.Encode as E
import qualified Data.Avro.Decode as D
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Avro.Schema as S
import Data.Avro.Types  as T
import Data.Avro.Deconflict as C
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Word
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text.Lazy as TL

decode :: FromAvro a => Schema -> ByteString -> Result a
decode sch bytes =
  case D.decodeAvro sch bytes of
    Right val -> fromAvro val
    Left err  -> Error err

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

class FromAvro a where
  fromAvro :: Value Type -> Result a

instance FromAvro Int64 where
  fromAvro (T.Long i) = pure i
  fromAvro (T.Int i)  = pure (fromIntegral i)
  fromAvro _          = fail "Invalid Avro value for Int64"

instance FromAvro a => FromAvro (Maybe a) where
  fromAvro (T.Union _ _ T.Null) = pure Nothing
  fromAvro (T.Union _ _ v) = Just <$> fromAvro v
  fromAvro _ = fail "Invalid Avro value for 'Maybe a'"

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
