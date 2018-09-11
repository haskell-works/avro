module Data.Avro.Decode.Lazy.Convert
where

import           Data.Avro.Decode.Lazy.LazyValue (LazyValue)
import qualified Data.Avro.Decode.Lazy.LazyValue as D
import           Data.Avro.Types.Value           (Value)
import qualified Data.Avro.Types.Value           as V
import           Data.Text                       (Text)

toStrictValue :: LazyValue f -> Either String (Value f)
toStrictValue d = case d of
  D.Null         -> Right V.Null
  D.Boolean v    -> Right $ V.Boolean v
  D.Int v        -> Right $ V.Int v
  D.Long v       -> Right $ V.Long v
  D.Float v      -> Right $ V.Float v
  D.Double v     -> Right $ V.Double v
  D.Bytes v      -> Right $ V.Bytes v
  D.String v     -> Right $ V.String v
  D.Array vs     -> V.Array <$> traverse toStrictValue vs
  D.Map vs       -> V.Map <$> traverse toStrictValue vs
  D.Record f vs  -> V.Record f <$> traverse toStrictValue vs
  D.Union fs f v -> V.Union fs f <$> toStrictValue v
  D.Fixed f v    -> Right $ V.Fixed f v
  D.Enum f i v   -> Right $ V.Enum f i v
  D.Error v      -> Left v
{-# INLINE toStrictValue #-}

fromStrictValue :: Value f -> LazyValue f
fromStrictValue d = case d of
  V.Null         -> D.Null
  V.Boolean v    -> D.Boolean v
  V.Int v        -> D.Int v
  V.Long v       -> D.Long v
  V.Float v      -> D.Float v
  V.Double v     -> D.Double v
  V.Bytes v      -> D.Bytes v
  V.String v     -> D.String v
  V.Array vs     -> D.Array $ fromStrictValue <$> vs
  V.Map vs       -> D.Map $ fromStrictValue <$> vs
  V.Record f vs  -> D.Record f $ fromStrictValue <$> vs
  V.Union fs f v -> D.Union fs f $ fromStrictValue v
  V.Fixed f v    -> D.Fixed f v
  V.Enum f i v   -> D.Enum f i v
{-# INLINE fromStrictValue #-}
