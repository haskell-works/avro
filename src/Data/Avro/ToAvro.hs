{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Avro.ToAvro

where

import           Control.Arrow        (first)
import           Data.Avro.HasAvroSchema
import           Data.Avro.Schema     as S
import           Data.Avro.Types      as T
import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HashMap
import           Data.Int
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Lazy       as TL
import           Data.Tagged
import qualified Data.Vector          as V
import           Data.Word

class HasAvroSchema a => ToAvro a where
  toAvro :: a -> T.Value Type

(.=)  :: ToAvro a => Text -> a -> (Text,T.Value Type)
(.=) nm val = (nm,toAvro val)

instance ToAvro Bool where
  toAvro = T.Boolean

instance ToAvro () where
  toAvro _ = T.Null

instance ToAvro Int where
  toAvro = T.Long . fromIntegral

instance ToAvro Int32 where
  toAvro = T.Int

instance ToAvro Int64 where
  toAvro = T.Long

instance ToAvro Double where
  toAvro = T.Double

instance ToAvro Float where
  toAvro = T.Float

instance ToAvro Text.Text where
  toAvro = T.String

instance ToAvro TL.Text where
  toAvro = T.String . TL.toStrict

instance ToAvro B.ByteString where
  toAvro = T.Bytes

instance ToAvro BL.ByteString where
  toAvro = T.Bytes . BL.toStrict

instance (ToAvro a, ToAvro b) => ToAvro (Either a b) where
  toAvro e =
    let sch@(l:|[r]) = options (schemaOf e)
    in case e of
         Left a  -> T.Union sch l (toAvro a)
         Right b -> T.Union sch r (toAvro b)

instance (ToAvro a) => ToAvro (Map.Map Text a) where
  toAvro = toAvro . HashMap.fromList . Map.toList

instance (ToAvro a) => ToAvro (HashMap.HashMap Text a) where
  toAvro = T.Map . HashMap.map toAvro

instance (ToAvro a) => ToAvro (Map.Map TL.Text a) where
  toAvro = toAvro . HashMap.fromList . map (first TL.toStrict) . Map.toList

instance (ToAvro a) => ToAvro (HashMap.HashMap TL.Text a) where
  toAvro = toAvro . HashMap.fromList . map (first TL.toStrict) . HashMap.toList

instance (ToAvro a) => ToAvro (Map.Map String a) where
  toAvro = toAvro . HashMap.fromList . map (first Text.pack) . Map.toList

instance (ToAvro a) => ToAvro (HashMap.HashMap String a) where
  toAvro = toAvro . HashMap.fromList . map (first Text.pack) . HashMap.toList

instance (ToAvro a) => ToAvro (Maybe a) where
  toAvro a =
    let sch@(l:|[r]) = options (schemaOf a)
    in case a of
      Nothing -> T.Union sch S.Null (toAvro ())
      Just v  -> T.Union sch r (toAvro v)

instance (ToAvro a) => ToAvro [a] where
  toAvro = T.Array . V.fromList . (toAvro <$>)

