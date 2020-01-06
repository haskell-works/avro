{-# LANGUAGE TupleSections #-}
module Data.Avro.Schema.Deconflict
  ( deconflict
  ) where

import           Control.Applicative ((<|>))
import           Data.Avro.Schema    as S
import           Data.Avro.Types     as T
import qualified Data.Foldable       as Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (find)
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map            as M
import           Data.Semigroup      ((<>))
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Data.Traversable    (forM)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- | @deconflict writer reader@ will produce a schema that can encode/decode
-- with the writer's schema into the form specified by the reader's schema.
-- The returned schema is lazy in that it may have errors (but those will not
-- get in the way of pure parsing).
deconflict :: Schema -- ^ Writer schema
           -> Schema -- ^ Reader schema
           -> Either String Schema
deconflict aTy bTy
  | aTy == bTy = pure aTy
deconflict (S.Array aTy) (S.Array bTy) =
  S.Array <$> deconflict aTy bTy
deconflict (S.Map aTy) (S.Map bTy) =
  S.Map <$> deconflict aTy bTy
deconflict a@S.Enum{} b@S.Enum{}
  | name a == name b && symbols b `contains` symbols a = pure S.Enum
    { name    = name a
    , aliases = aliases a <> aliases b
    , doc     = doc a
    , symbols = symbols a
    }
deconflict a@S.Fixed {} b@S.Fixed {}
  | name a == name b && size a == size b = pure b
    { name    = name a
    , aliases = aliases a <> aliases b
    , size    = size a
    }
deconflict a@S.Record {} b@S.Record {}
  | name a == name b && order b `moreSpecified` order a = do
    fields' <- deconflictFields (fields a) (fields b)
    pure S.Record
      { name    = name a
      , aliases = aliases a <> aliases b
      , doc     = doc a
      , order   = order b
      , fields  = fields'
      }
deconflict (S.Union xs) (S.Union ys) =
  let err x = "Incorrect payload: union " <> (show . Foldable.toList $ typeName <$> ys) <> " does not contain schema " <> Text.unpack (typeName x)
  in S.Union <$> V.mapM (\x -> maybe (Left $ err x) (deconflict x) (findType x ys)) xs
deconflict nonUnion (S.Union ys)
  | Just y <- findType nonUnion ys =
    S.FreeUnion <$> deconflict nonUnion y
deconflict from (S.Int l) | isIntIn from = pure (S.Int l)
deconflict from to | isIntIn   from && isLongOut   to = pure S.IntLongCoercion
deconflict from to | isIntIn   from && isFloatOut  to = pure S.IntFloatCoercion
deconflict from to | isIntIn   from && isDoubleOut to = pure S.IntDoubleCoercion
deconflict from to | isLongIn  from && isLongOut   to = pure (S.Long Nothing)
deconflict from to | isLongIn  from && isFloatOut  to = pure S.LongFloatCoercion
deconflict from to | isLongIn  from && isDoubleOut to = pure S.LongDoubleCoercion
deconflict from to | isFloatIn from && isFloatOut  to = pure S.Float
deconflict from to | isFloatIn from && isDoubleOut to = pure S.FloatDoubleCoercion
deconflict S.Double to | isDoubleOut to = pure S.Double
deconflict (S.Bytes _)  (S.String l) = pure (S.String l)  -- These are free coercions
deconflict (S.String _) (S.Bytes l)  = pure (S.Bytes l)   -- These are free coercions
deconflict (S.FreeUnion a) b = deconflict a b -- Free unions are free to discard
deconflict a (S.FreeUnion b) = deconflict a b -- Free unions are free to discard
deconflict a b = Left $ "Can not resolve differing writer and reader schemas: " ++ show (a, b)

isIntIn :: Schema -> Bool
isIntIn (S.Int _)           = True
isIntIn S.IntLongCoercion   = True
isIntIn S.IntFloatCoercion  = True
isIntIn S.IntDoubleCoercion = True
isIntIn _                   = False

isLongIn :: Schema -> Bool
isLongIn (S.Long _)           = True
isLongIn S.LongFloatCoercion  = True
isLongIn S.LongDoubleCoercion = True
isLongIn _                    = False

isFloatIn :: Schema -> Bool
isFloatIn S.Float               = True
isFloatIn S.FloatDoubleCoercion = True
isFloatIn _                     = False

isLongOut :: Schema -> Bool
isLongOut (S.Long _)        = True
isLongOut S.IntLongCoercion = True
isLongOut _                 = False

isFloatOut :: Schema -> Bool
isFloatOut S.Float             = True
isFloatOut S.IntFloatCoercion  = True
isFloatOut S.LongFloatCoercion = True
isFloatOut _                   = False

isDoubleOut :: Schema -> Bool
isDoubleOut S.Double              = True
isDoubleOut S.IntDoubleCoercion   = True
isDoubleOut S.LongDoubleCoercion  = True
isDoubleOut S.FloatDoubleCoercion = True
isDoubleOut _                     = False

moreSpecified :: Maybe Order -> Maybe Order -> Bool
moreSpecified _ Nothing                           = True
moreSpecified _ (Just Ignore)                     = True
moreSpecified (Just Ascending)  (Just Ascending)  = True
moreSpecified (Just Descending) (Just Descending) = True
moreSpecified _ _                                 = False

contains :: V.Vector Text -> V.Vector Text -> Bool
contains container elts =
  and [e `V.elem` container | e <- V.toList elts]

-- For each field:
--  1) If it exists in both schemas, deconflict it
--  2) If it's only in the reader schema and has a default, mark it defaulted.
--  2) If it's only in the reader schema and has no default, fail.
--  3) If it's only in the writer schema, mark it ignored.
deconflictFields :: [Field] -> [Field] -> Either String [Field]
deconflictFields writerFields readerFields =
  sequence $ (deconflictField <$> writerFields) <> defaultedFields
  where
    defaultedFields = [confirmDefaulted f | f <- readerFields, findField f writerFields == Nothing]

    confirmDefaulted :: Field -> Either String Field
    confirmDefaulted f
      | Just def <- fldDefault f = pure f { fldStatus = Defaulted }
      | otherwise = Left $ "No default found for deconflicted field " <> Text.unpack (fldName f)

    deconflictField :: Field -> Either String Field
    deconflictField writerField
      | Just readerField <- findField writerField readerFields = do
        t <- deconflict (fldType writerField) (fldType readerField)
        pure writerField { fldType = t }
      | otherwise =
        pure writerField { fldStatus = Ignored, fldDefault = Nothing }

findField :: Field -> [Field] -> Maybe Field
findField f fs =
  let
    byName = find (\x -> fldName x == fldName f) fs
    allNames fld = Set.fromList (fldName fld : fldAliases fld)
    fNames = allNames f
    sameField = not . Set.null . Set.intersection fNames . allNames
    byAliases = find sameField fs
  in byName <|> byAliases

findType :: Foldable f => Schema -> f Schema -> Maybe Schema
findType schema =
  let tn = typeName schema
  in Foldable.find ((tn ==) . typeName) -- TODO: Consider aliases
