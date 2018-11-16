{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Avro.Decode.Lazy.FromLazyAvro

where

import           Control.Arrow                   (first)
import           Data.Avro.Decode.Lazy.LazyValue as T
import qualified Data.Avro.Encode                as E
import           Data.Avro.HasAvroSchema
import           Data.Avro.Schema                as S
import qualified Data.ByteString                 as B
import           Data.ByteString.Lazy            (ByteString)
import qualified Data.ByteString.Lazy            as BL
import           Data.Foldable                   (toList)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Int
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.Map                        as Map
import           Data.Monoid                     ((<>))
import           Data.Tagged
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy                  as TL
import qualified Data.Vector                     as V
import qualified Data.Vector.Unboxed             as U
import           Data.Word

class HasAvroSchema a => FromLazyAvro a where
  fromLazyAvro :: LazyValue Type -> Result a

(.~:) :: FromLazyAvro a => HashMap.HashMap Text (LazyValue Type) -> Text -> Result a
(.~:) obj key =
  case HashMap.lookup key obj of
    Nothing -> fail $ "Requested field not available: " <> show key
    Just v  -> fromLazyAvro v

instance (FromLazyAvro a, FromLazyAvro b) => FromLazyAvro (Either a b) where
  fromLazyAvro e@(T.Union _ branch x)
    | S.matches branch schemaA = Left  <$> fromLazyAvro x
    | S.matches branch schemaB = Right <$> fromLazyAvro x
    | otherwise              = badLazyValue e "either"
    where Tagged schemaA = schema :: Tagged a Type
          Tagged schemaB = schema :: Tagged b Type
  fromLazyAvro x = badLazyValue x "either"
instance FromLazyAvro Bool where
  fromLazyAvro (T.Boolean b) = pure b
  fromLazyAvro v             = badLazyValue v "Bool"
instance FromLazyAvro B.ByteString where
  fromLazyAvro (T.Bytes b) = pure b
  fromLazyAvro v           = badLazyValue v "ByteString"
instance FromLazyAvro BL.ByteString where
  fromLazyAvro (T.Bytes b) = pure (BL.fromStrict b)
  fromLazyAvro v           = badLazyValue v "Lazy ByteString"
instance FromLazyAvro Int where
  fromLazyAvro (T.Int i) | (fromIntegral i :: Integer) < fromIntegral (maxBound :: Int)
                      = pure (fromIntegral i)
  fromLazyAvro (T.Long i) | (fromIntegral i :: Integer) < fromIntegral (maxBound :: Int)
                      = pure (fromIntegral i)
  fromLazyAvro v          = badLazyValue v "Int"
instance FromLazyAvro Int32 where
  fromLazyAvro (T.Int i) = pure (fromIntegral i)
  fromLazyAvro v         = badLazyValue v "Int32"
instance FromLazyAvro Int64 where
  fromLazyAvro (T.Long i) = pure i
  fromLazyAvro (T.Int i)  = pure (fromIntegral i)
  fromLazyAvro v          = badLazyValue v "Int64"
instance FromLazyAvro Double where
  fromLazyAvro (T.Double d) = pure d
  fromLazyAvro v            = badLazyValue v "Double"

instance FromLazyAvro Float where
  fromLazyAvro (T.Float f) = pure f
  fromLazyAvro v           = badLazyValue v "Float"

instance FromLazyAvro a => FromLazyAvro (Maybe a) where
  fromLazyAvro (T.Union (S.Null :| [_])  _ T.Null) = pure Nothing
  fromLazyAvro (T.Union (S.Null :| [_]) _ v)       = Just <$> fromLazyAvro v
  fromLazyAvro v                                   = badLazyValue v "Maybe a"

instance FromLazyAvro a => FromLazyAvro [a] where
  fromLazyAvro (T.Array vec) = mapM fromLazyAvro $ toList vec
  fromLazyAvro v             = badLazyValue v "[a]"

instance FromLazyAvro a => FromLazyAvro (V.Vector a) where
  fromLazyAvro (T.Array vec) = mapM fromLazyAvro vec
  fromLazyAvro v             = badLazyValue v "Vector a"

instance (U.Unbox a, FromLazyAvro a) => FromLazyAvro (U.Vector a) where
  fromLazyAvro (T.Array vec) = U.convert <$> mapM fromLazyAvro vec
  fromLazyAvro v             = badLazyValue v "Unboxed Vector a"

instance FromLazyAvro Text where
  fromLazyAvro (T.String txt) = pure txt
  fromLazyAvro v              = badLazyValue v "Text"

instance FromLazyAvro TL.Text where
  fromLazyAvro (T.String txt) = pure (TL.fromStrict txt)
  fromLazyAvro v              = badLazyValue v "Lazy Text"

instance (FromLazyAvro a) => FromLazyAvro (Map.Map Text a) where
  fromLazyAvro (T.Record _ mp) = mapM fromLazyAvro $ Map.fromList (HashMap.toList mp)
  fromLazyAvro (T.Map mp)      = mapM fromLazyAvro $ Map.fromList (HashMap.toList mp)
  fromLazyAvro v               = badLazyValue v "Map Text a"

instance (FromLazyAvro a) => FromLazyAvro (HashMap.HashMap Text a) where
  fromLazyAvro (T.Record _ mp) = mapM fromLazyAvro mp
  fromLazyAvro (T.Map mp)      = mapM fromLazyAvro mp
  fromLazyAvro v               = badLazyValue v "HashMap Text a"

badLazyValue :: LazyValue Type -> String -> Result a
badLazyValue v t = fail $ "Unexpected value when decoding for '" <> t <> "': " <> show v
