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
           -> Schema
deconflict writerSchema readerSchema = go writerSchema readerSchema
  where
    go :: Schema -> Schema -> Schema
    go aTy bTy
      | aTy == bTy = aTy
    go (S.Array aTy) (S.Array bTy) =
      S.Array $ go aTy bTy
    go (S.Map aTy) (S.Map bTy) =
      S.Map $ go aTy bTy
    go a@S.Enum{} b@S.Enum{}
      | name a == name b && symbols b `contains` symbols a = S.Enum
        { name    = name a
        , aliases = aliases a <> aliases b
        , doc     = doc a
        , symbols = symbols a
        }
    go a@S.Fixed {} b@S.Fixed {}
      | name a == name b && size a == size b = S.Fixed
        { name    = name a
        , aliases = aliases a <> aliases b
        , size    = size a
        }
    go a@S.Record {} b@S.Record {}
      | name a == name b && order b `moreSpecified` order a =
        let fields' = deconflictFields (fields a) (fields b)
        in S.Record
          { name    = name a
          , aliases = aliases a <> aliases b
          , doc     = doc a
          , order   = order b
          , fields  = fields'
          }
    go (S.Union xs) (S.Union ys) =
      let err x = S.Panic x $ "Incorrect payload: union " <> (show . Foldable.toList $ typeName <$> ys) <> " does not contain schema " <> Text.unpack (typeName x)
      in S.Union $ (\x -> maybe (err x) (go x) (findType x ys)) <$> xs
    go nonUnion (S.Union ys)
      | Just y <- findType nonUnion ys =
        S.FreeUnion (go nonUnion y)
    go from S.Int | isIntIn from = S.Int
    go from to | isIntIn   from && isLongOut   to = S.IntLongCoercion
    go from to | isIntIn   from && isFloatOut  to = S.IntFloatCoercion
    go from to | isIntIn   from && isDoubleOut to = S.IntDoubleCoercion
    go from to | isLongIn  from && isLongOut   to = S.Long
    go from to | isLongIn  from && isFloatOut  to = S.LongFloatCoercion
    go from to | isLongIn  from && isDoubleOut to = S.LongDoubleCoercion
    go from to | isFloatIn from && isFloatOut  to = S.Float
    go from to | isFloatIn from && isDoubleOut to = S.FloatDoubleCoercion
    go S.Double to | isDoubleOut to = S.Double
    go S.Bytes  S.String = S.Bytes  -- These are free coercions
    go S.String S.Bytes  = S.String -- These are free coercions
    go (S.FreeUnion a) b = go a b -- Free unions are free to discard
    go a (S.FreeUnion b) = go a b -- Free unions are free to discard
    go a b = S.Panic a $ "Can not resolve differing writer and reader schemas: " ++ show (a, b)

isIntIn :: Schema -> Bool
isIntIn S.Int               = True
isIntIn S.IntLongCoercion   = True
isIntIn S.IntFloatCoercion  = True
isIntIn S.IntDoubleCoercion = True
isIntIn _                   = False

isLongIn :: Schema -> Bool
isLongIn S.Long               = True
isLongIn S.LongFloatCoercion  = True
isLongIn S.LongDoubleCoercion = True
isLongIn _                    = False

isFloatIn :: Schema -> Bool
isFloatIn S.Float               = True
isFloatIn S.FloatDoubleCoercion = True
isFloatIn _                     = False

isLongOut :: Schema -> Bool
isLongOut S.Long            = True
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
moreSpecified _ Nothing       = True
moreSpecified _ (Just Ignore) = True
moreSpecified (Just Ascending)  (Just Ascending)  = True
moreSpecified (Just Descending) (Just Descending) = True
moreSpecified _ _ = False

contains :: V.Vector Text -> V.Vector Text -> Bool
contains container elts =
  and [e `V.elem` container | e <- V.toList elts]

-- For each field:
--  1) If it exists in both schemas, deconflict it
--  2) If it's only in the reader schema and has a default, mark it defaulted.
--  2) If it's only in the reader schema and has no default, set its type to Panic.
--  3) If it's only in the writer schema, mark it ignored.
deconflictFields :: [Field] -> [Field] -> [Field]
deconflictFields writerFields readerFields =
  (deconflictField <$> writerFields) <> defaultedFields
  where
    defaultedFields = [makeDefaulted f | f <- readerFields, findField f writerFields == Nothing]

    makeDefaulted :: Field -> Field
    makeDefaulted f
      | Just def <- fldDefault f = f { fldStatus = Defaulted def }
      | otherwise = f { fldType =
        S.Panic (fldType f) ("No default found for deconflicted field "<>Text.unpack (fldName f)) }

    deconflictField :: Field -> Field
    deconflictField writerField
      | Just readerField <- findField writerField readerFields =
        writerField { fldType = deconflict (fldType writerField) (fldType readerField) }
      | otherwise =
        writerField { fldStatus = Ignored }

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
