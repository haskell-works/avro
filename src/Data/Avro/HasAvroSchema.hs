{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Avro.HasAvroSchema where

import           Data.Avro.Schema     as S
import           Data.Avro.Types      as T
import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HashMap
import           Data.Int
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text.Lazy       as TL
import           Data.Tagged
import qualified Data.Vector          as V
import           Data.Word
import Data.Proxy

class HasAvroSchema a where
  schema :: Tagged a Type

schemaOf :: (HasAvroSchema a) => a -> Type
schemaOf = witness schema

instance HasAvroSchema Bool where
  schema = Tagged S.Boolean

instance HasAvroSchema () where
  schema = Tagged S.Null

instance HasAvroSchema Int where
  schema = Tagged S.Long

instance HasAvroSchema Int32 where
  schema = Tagged S.Int

instance HasAvroSchema Int64 where
  schema = Tagged S.Long

instance HasAvroSchema Double where
  schema = Tagged S.Double

instance HasAvroSchema Float where
  schema = Tagged S.Float

instance HasAvroSchema Text.Text where
  schema = Tagged S.String

instance HasAvroSchema TL.Text where
  schema = Tagged S.String

instance HasAvroSchema B.ByteString where
  schema = Tagged S.Bytes

instance HasAvroSchema BL.ByteString where
  schema = Tagged S.Bytes

instance (HasAvroSchema a, HasAvroSchema b) => HasAvroSchema (Either a b) where
  schema = Tagged $ mkUnion (untag (schema :: Tagged a Type) :| [untag (schema :: Tagged b Type)])

instance (HasAvroSchema a) => HasAvroSchema (Map.Map Text a) where
  schema = wrapTag S.Map (schema :: Tagged a Type)

instance (HasAvroSchema a) => HasAvroSchema (HashMap.HashMap Text a) where
  schema = wrapTag S.Map (schema :: Tagged a Type)

instance (HasAvroSchema a) => HasAvroSchema (Map.Map TL.Text a) where
  schema = wrapTag S.Map (schema :: Tagged a Type)

instance (HasAvroSchema a) => HasAvroSchema (HashMap.HashMap TL.Text a) where
  schema = wrapTag S.Map (schema :: Tagged a Type)

instance (HasAvroSchema a) => HasAvroSchema (Map.Map String a) where
  schema = wrapTag S.Map (schema :: Tagged a Type)

instance (HasAvroSchema a) => HasAvroSchema (HashMap.HashMap String a) where
  schema = wrapTag S.Map (schema :: Tagged a Type)

instance (HasAvroSchema a) => HasAvroSchema (Maybe a) where
  schema = Tagged $ mkUnion (S.Null:| [untag (schema :: Tagged a Type)])

instance (HasAvroSchema a) => HasAvroSchema [a] where
  schema = wrapTag S.Array (schema :: Tagged a Type)

wrapTag :: (Type -> Type) -> Tagged a Type -> Tagged b Type
wrapTag f = Tagged . f . untag
{-# INLINE wrapTag #-}
