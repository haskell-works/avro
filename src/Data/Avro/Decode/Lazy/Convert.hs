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
  D.Int s v      -> Right $ V.Int s v
  D.Long s v     -> Right $ V.Long s v
  D.Float s v    -> Right $ V.Float s v
  D.Double s v   -> Right $ V.Double s v
  D.Bytes s v    -> Right $ V.Bytes s v
  D.String s v   -> Right $ V.String s v
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
  V.Int s v      -> D.Int s v
  V.Long s v     -> D.Long s v
  V.Float s v    -> D.Float s v
  V.Double s v   -> D.Double s v
  V.Bytes s v    -> D.Bytes s v
  V.String s v   -> D.String s v
  V.Array vs     -> D.Array $ fromStrictValue <$> vs
  V.Map vs       -> D.Map $ fromStrictValue <$> vs
  V.Record f vs  -> D.Record f $ fromStrictValue <$> vs
  V.Union fs f v -> D.Union fs f $ fromStrictValue v
  V.Fixed f v    -> D.Fixed f v
  V.Enum f i v   -> D.Enum f i v
{-# INLINE fromStrictValue #-}
