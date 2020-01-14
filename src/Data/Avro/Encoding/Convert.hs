{-# LANGUAGE LambdaCase #-}
module Data.Avro.Encoding.Convert
where

import           Data.Avro.Encoding.Value
import           Data.Avro.Schema            (Schema, fields, fldName)
import qualified Data.Avro.Schema            as Schema
import           Data.Avro.Schema.ReadSchema (fromSchema)
import qualified Data.Avro.Types.Value       as Old
import qualified Data.HashMap.Strict         as HashMap
import           Data.Vector                 as V

-- | This function will be unnecessary when we fully migrate to 'Value'
convertValue :: Old.Value Schema -> Value
convertValue = \case
  Old.Null -> Null
  Old.Boolean v       -> Boolean v
  Old.Int s v         -> Int (fromSchema s) v
  Old.Long s v        -> Long (fromSchema s) v
  Old.Float s v       -> Float (fromSchema s) v
  Old.Double s v      -> Double (fromSchema s) v
  Old.Bytes s v       -> Bytes (fromSchema s) v
  Old.String s v      -> String (fromSchema s) v
  Old.Array v         -> Array $ fmap convertValue v
  Old.Map v           -> Map $ fmap convertValue v
  Old.Fixed s v       -> Fixed (fromSchema s) v
  Old.Enum s i v      -> Enum (fromSchema s) i v
  Old.Union vs sch v  ->
    case V.elemIndex sch vs of
      Just ix -> Union (fromSchema sch) ix (convertValue v)
      Nothing -> error "Union contains a value of an unknown schema"
  Old.Record sch vs   ->
    let
      fldNames = fldName <$> fields sch
      values = fmap (\n -> convertValue $ vs HashMap.! n) fldNames
    in Record $ V.fromList values
