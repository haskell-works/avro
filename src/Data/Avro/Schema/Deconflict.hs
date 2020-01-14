{-# LANGUAGE TupleSections #-}
module Data.Avro.Schema.Deconflict
  ( deconflict
  , deconflict'
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
import           Data.Maybe          (isNothing)
import           Data.Semigroup      ((<>))
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Data.Traversable    (forM)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

import           Data.Avro.Schema.ReadSchema (ReadField, ReadSchema)
import qualified Data.Avro.Schema.ReadSchema as Read

deconflict' :: Schema -> Schema -> Either String ReadSchema
deconflict' writerSchema readerSchema | writerSchema == readerSchema = pure (Read.fromSchema readerSchema)
deconflict' S.Null S.Null             = pure Read.Null
deconflict' S.Boolean S.Boolean       = pure Read.Boolean

deconflict' (S.Int _) (S.Int r)       = pure (Read.Int r)
deconflict' (S.Int _) (S.Long r)      = pure (Read.Long Read.LongFromInt r)
deconflict' (S.Int _) S.Float         = pure (Read.Float Read.FloatFromInt)
deconflict' (S.Int _) S.Double        = pure (Read.Double Read.DoubleFromInt)

deconflict' (S.Long _) (S.Long r)     = pure (Read.Long Read.ReadLong r)
deconflict' (S.Long _) S.Float        = pure (Read.Float Read.FloatFromLong)
deconflict' (S.Long _) S.Double       = pure (Read.Double Read.DoubleFromLong)

deconflict' S.Float S.Float           = pure (Read.Float Read.ReadFloat)
deconflict' S.Float S.Double          = pure (Read.Double Read.DoubleFromFloat)

deconflict' S.Double S.Double         = pure (Read.Double Read.ReadDouble)

deconflict' (S.Bytes _) (S.Bytes r)   = pure (Read.Bytes r)
deconflict' (S.Bytes _) (S.String r)  = pure (Read.String r)

deconflict' (S.String _) (S.String r) = pure (Read.String r)
deconflict' (S.String _) (S.Bytes r)  = pure (Read.Bytes r)

deconflict' (S.Array w) (S.Array r)   = Read.Array <$> deconflict' w r

deconflict' (S.Map w) (S.Map r)       = Read.Map <$> deconflict' w r

deconflict' w@S.Enum{} r@S.Enum{}
  | name w == name r && symbols w `contains` symbols r = pure Read.Enum
    { Read.name    = name r
    , Read.aliases = aliases w <> aliases r
    , Read.doc     = doc r
    , Read.symbols = symbols w
    }

deconflict' w@S.Fixed {} r@S.Fixed {}
  | name w == name r && size w == size r = pure Read.Fixed
    { Read.name    = name r
    , Read.aliases = aliases w <> aliases r
    , Read.size    = size w
    , Read.logicalTypeF = logicalTypeF r
    }

deconflict' w@S.Record {} r@S.Record {}
  | name w == name r && order r `moreSpecified` order w = do
    fields' <- deconflictFields' (fields w) (fields r)
    pure Read.Record
      { Read.name    = name r
      , Read.aliases = aliases w <> aliases r
      , Read.doc     = doc r
      , Read.order   = order r
      , Read.fields  = fields'
      }

deconflict' (S.Union ws) (S.Union rs) =
  let
    ws' = extractValues ws
    rs' = extractValues rs
    err x = "Incorrect payload: union " <> (show . Foldable.toList $ typeName <$> rs') <> " does not contain schema " <> Text.unpack (typeName x)
  in Read.Union . Indexed <$> V.mapM (\w -> maybe (Left $ err w) (\(i, r') -> (i,) <$> deconflict' w r') (findTypeV w rs')) ws'

deconflict' nonUnion (S.Union rs)
  | Just (ix, y) <- findTypeV nonUnion (extractValues rs) =
    Read.FreeUnion ix <$> deconflict' nonUnion y

deconflict' a b = Left $ "Can not resolve differing writer and reader schemas: " ++ show (a, b)

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
  let
    xs' = extractValues xs
    ys' = extractValues ys
    err x = "Incorrect payload: union " <> (show . Foldable.toList $ typeName <$> ys') <> " does not contain schema " <> Text.unpack (typeName x)
  in S.Union . Indexed <$> V.mapM (\x -> maybe (Left $ err x) (\(i, y') -> (i,) <$> deconflict x y') (findTypeV x ys')) xs'
deconflict nonUnion (S.Union ys)
  | Just (ix, y) <- findTypeV nonUnion (extractValues ys) =
    S.FreeUnion ix <$> deconflict nonUnion y
deconflict from (S.Int l) | isIntIn from = pure (S.Int l)
deconflict (S.Int l) (S.Long _) = pure (S.Int l)
-- deconflict from to | isIntIn   from && isLongOut   to = pure S.IntLongCoercion
-- deconflict from to | isIntIn   from && isFloatOut  to = pure S.IntFloatCoercion
-- deconflict from to | isIntIn   from && isDoubleOut to = pure S.IntDoubleCoercion
deconflict from to | isLongIn  from && isLongOut   to = pure (S.Long Nothing)
-- deconflict from to | isLongIn  from && isFloatOut  to = pure S.LongFloatCoercion
-- deconflict from to | isLongIn  from && isDoubleOut to = pure S.LongDoubleCoercion
deconflict from to | isFloatIn from && isFloatOut  to = pure S.Float
-- deconflict from to | isFloatIn from && isDoubleOut to = pure S.FloatDoubleCoercion
deconflict S.Double to | isDoubleOut to = pure S.Double
deconflict (S.Bytes _)  (S.String l) = pure (S.String l)  -- These are free coercions
deconflict (S.String _) (S.Bytes l)  = pure (S.Bytes l)   -- These are free coercions
deconflict (S.FreeUnion _ a) b = deconflict a b -- Free unions are free to discard
deconflict a (S.FreeUnion _ b) = deconflict a b -- Free unions are free to discard
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
    defaultedFields = [uncurry confirmDefaulted f | f <- zip [0..] readerFields, isNothing (findField (snd f) writerFields)]

    confirmDefaulted :: Int -> Field -> Either String Field
    confirmDefaulted ix f
      | Just def <- fldDefault f = pure f { fldStatus = Defaulted ix def }
      | otherwise = Left $ "No default found for deconflicted field " <> Text.unpack (fldName f)

    deconflictField :: Field -> Either String Field
    deconflictField writerField
      | Just readerField <- findField writerField readerFields = do
        t <- deconflict (fldType writerField) (fldType readerField)
        pure writerField { fldType = t, fldDefault = fldDefault readerField}
      | otherwise =
        pure writerField { fldStatus = Ignored, fldDefault = Nothing }

-- For each field:
--  1) If it exists in both schemas, deconflict it
--  2) If it's only in the reader schema and has a default, mark it defaulted.
--  2) If it's only in the reader schema and has no default, fail.
--  3) If it's only in the writer schema, mark it ignored.
deconflictFields' :: [Field] -> [Field] -> Either String [ReadField]
deconflictFields' writerFields readerFields =
  sequence $ (deconflictField <$> writerFields) <> defaultedFields
  where
    defaultedFields = [uncurry confirmDefaulted f | f <- zip [0..] readerFields, isNothing (findField (snd f) writerFields)]

    confirmDefaulted :: Int -> Field -> Either String ReadField
    confirmDefaulted ix f
      | Just def <- fldDefault f = pure $ (Read.fromField f) { Read.fldStatus = Defaulted ix def }
      | otherwise = Left $ "No default found for deconflicted field " <> Text.unpack (fldName f)

    deconflictField :: Field -> Either String ReadField
    deconflictField writerField
      | Just readerField <- findField writerField readerFields = do
        t <- deconflict' (fldType writerField) (fldType readerField)
        pure (Read.fromField writerField) { Read.fldType = t, Read.fldDefault = fldDefault readerField}
      | otherwise =
        pure $ (Read.fromField writerField) { Read.fldStatus = Ignored, Read.fldDefault = Nothing }

findField :: Field -> [Field] -> Maybe Field
findField f fs =
  let
    setStatus f' = f' {fldStatus = fldStatus f}
    byName = find (\x -> fldName x == fldName f) fs
    allNames fld = Set.fromList (fldName fld : fldAliases fld)
    fNames = allNames f
    sameField = not . Set.null . Set.intersection fNames . allNames
    byAliases = find sameField fs
  in fmap setStatus (byName <|> byAliases)

findType :: Foldable f => Schema -> f Schema -> Maybe Schema
findType schema =
  let tn = typeName schema
  in Foldable.find ((tn ==) . typeName) -- TODO: Consider aliases

-- replaceUnionType :: Vector Schema -> Schema -> (Schema -> Either String Schema) -> Either String (Vector Schema)
-- replaceUnionType schemas toFind update =

findTypeV :: Schema -> Vector Schema -> Maybe (Int, Schema)
findTypeV schema schemas =
  let tn = typeName schema
  in case V.findIndex ((tn ==) . typeName) schemas of
      Just ix -> Just (ix, V.unsafeIndex schemas ix)
