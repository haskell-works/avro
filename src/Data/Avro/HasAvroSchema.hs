{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Avro.HasAvroSchema where

import qualified Data.Array           as Ar
import           Data.Avro.Schema     as S
import           Data.Avro.Types      as T
import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HashMap
import           Data.Int
import           Data.Ix              (Ix)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Proxy
import qualified Data.Set             as S
import           Data.Tagged
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Lazy       as TL
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as U
import           Data.Word

class HasAvroSchema a where
  schema :: Tagged a Type

schemaOf :: (HasAvroSchema a) => a -> Type
schemaOf = witness schema

instance HasAvroSchema Word8 where
  schema = Tagged S.Int

instance HasAvroSchema Word16 where
  schema = Tagged S.Int

instance HasAvroSchema Word32 where
  schema = Tagged S.Long

instance HasAvroSchema Word64 where
  schema = Tagged S.Long

instance HasAvroSchema Bool where
  schema = Tagged S.Boolean

instance HasAvroSchema () where
  schema = Tagged S.Null

instance HasAvroSchema Int where
  schema = Tagged S.Long

instance HasAvroSchema Int8 where
  schema = Tagged S.Int

instance HasAvroSchema Int16 where
  schema = Tagged S.Int

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
  schema = Tagged $ S.Union $ V.fromListN 2 [untag (schema :: Tagged a Type), untag (schema :: Tagged b Type)]

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

instance (HasAvroSchema a, Ix i) => HasAvroSchema (Ar.Array i a) where
  schema = wrapTag S.Array (schema :: Tagged a Type)

instance HasAvroSchema a => HasAvroSchema (V.Vector a) where
  schema = wrapTag S.Array (schema :: Tagged a Type)

instance HasAvroSchema a => HasAvroSchema (U.Vector a) where
  schema = wrapTag S.Array (schema :: Tagged a Type)

instance HasAvroSchema a => HasAvroSchema (S.Set a) where
  schema = wrapTag S.Array (schema :: Tagged a Type)

wrapTag :: (Type -> Type) -> Tagged a Type -> Tagged b Type
wrapTag f = Tagged . f . untag
{-# INLINE wrapTag #-}
