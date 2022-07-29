{-# LANGUAGE TupleSections #-}
module Data.Avro.Schema.Deconflict
  ( deconflict
  ) where

import           Control.Applicative     ((<|>))
import           Data.Avro.Schema.Schema as S
import qualified Data.Foldable           as Foldable
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.List               (find)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as M
import           Data.Maybe              (isNothing)
import           Data.Semigroup          ((<>))
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           Data.Avro.Schema.ReadSchema (FieldStatus (..), ReadField, ReadSchema)
import qualified Data.Avro.Schema.ReadSchema as Read

import Debug.Trace

-- | @deconflict writer reader@ will produce a schema that can decode
-- with the writer's schema into the form specified by the reader's schema.
--
-- Schema resolution rules are described by the specification: <https://avro.apache.org/docs/current/spec.html#Schema+Resolution>
deconflict :: Schema -> Schema -> Either String ReadSchema
deconflict writerSchema readerSchema | writerSchema == readerSchema = pure (Read.fromSchema readerSchema)
deconflict S.Null S.Null             = pure Read.Null
deconflict S.Boolean S.Boolean       = pure Read.Boolean

deconflict (S.Int _) (S.Int r)       = pure (Read.Int r)
deconflict (S.Int _) (S.Long r)      = pure (Read.Long Read.LongFromInt r)
deconflict (S.Int _) S.Float         = pure (Read.Float Read.FloatFromInt)
deconflict (S.Int _) S.Double        = pure (Read.Double Read.DoubleFromInt)

deconflict (S.Long _) (S.Long r)     = pure (Read.Long Read.ReadLong r)
deconflict (S.Long _) S.Float        = pure (Read.Float Read.FloatFromLong)
deconflict (S.Long _) S.Double       = pure (Read.Double Read.DoubleFromLong)

deconflict S.Float S.Float           = pure (Read.Float Read.ReadFloat)
deconflict S.Float S.Double          = pure (Read.Double Read.DoubleFromFloat)

deconflict S.Double S.Double         = pure (Read.Double Read.ReadDouble)

deconflict (S.Bytes _) (S.Bytes r)   = pure (Read.Bytes r)
deconflict (S.Bytes _) (S.String r)  = pure (Read.String r)

deconflict (S.String _) (S.String r) = pure (Read.String r)
deconflict (S.String _) (S.Bytes r)  = pure (Read.Bytes r)

deconflict (S.Array w) (S.Array r)   = Read.Array <$> deconflict w r

deconflict (S.Map w) (S.Map r)       = Read.Map <$> deconflict w r

deconflict w@S.Enum{} r@S.Enum{}
  | name w == name r && symbols w `contains` symbols r = pure Read.Enum
    { Read.name    = name r
    , Read.aliases = aliases w <> aliases r
    , Read.doc     = doc r
    , Read.symbols = symbols w
    }

deconflict w@S.Fixed {} r@S.Fixed {}
  | name w == name r && size w == size r = pure Read.Fixed
    { Read.name    = name r
    , Read.aliases = aliases w <> aliases r
    , Read.size    = size w
    , Read.logicalTypeF = logicalTypeF r
    }

deconflict w@S.Record {} r@S.Record {}
  | name w == name r = do
    fields' <- deconflictFields (fields w) (fields r)
    pure Read.Record
      { Read.name    = name r
      , Read.aliases = aliases w <> aliases r
      , Read.doc     = doc r
      , Read.fields  = fields'
      }

deconflict (S.Union ws) (S.Union rs) =
  let
    err x = "Incorrect payload: union " <> (show . Foldable.toList $ typeName <$> rs) <> " does not contain schema " <> Text.unpack (typeName x)
  in Read.Union <$> V.mapM (\w -> maybe (Left $ err w) (\(i, r') -> (i,) <$> deconflict w r') (findTypeV w rs)) ws

deconflict nonUnion (S.Union rs)
  | Just (ix, y) <- findTypeV nonUnion rs =
    Read.FreeUnion ix <$> deconflict nonUnion y

deconflict a b = Left $ "Can not resolve differing writer and reader schemas: " ++ show (a, b)

contains :: V.Vector Text -> V.Vector Text -> Bool
contains container elts =
  and [e `V.elem` container | e <- V.toList elts]

-- For each field:
--  1) If it exists in both schemas, deconflict it
--  2) If it's only in the reader schema and has a default, mark it defaulted.
--  2) If it's only in the reader schema and has no default, fail.
--  3) If it's only in the writer schema, mark it ignored.
deconflictFields :: [Field] -> [Field] -> Either String [ReadField]
deconflictFields writerFields readerFields =
  sequence $ (deconflictField <$> writerFields) <> defaultedFields
  where
    indexedReaderFields = zip [0..] readerFields
    defaultedFields = [uncurry confirmDefaulted f | f <- indexedReaderFields, isNothing (findField (snd f) (zip [0..] writerFields))]

    confirmDefaulted :: Int -> Field -> Either String ReadField
    confirmDefaulted ix f
      | Just def <- fldDefault f = pure $ Read.fromField (Defaulted ix def) f
      | otherwise = Left $ "No default found for deconflicted field " <> Text.unpack (fldName f)

    deconflictField :: Field -> Either String ReadField
    deconflictField writerField
      | Just (ix, readerField) <- findField writerField indexedReaderFields = do
        t <- deconflict (fldType writerField) (fldType readerField)
        pure (Read.fromField (AsIs ix) writerField) { Read.fldType = t, Read.fldDefault = fldDefault readerField}
      | otherwise =
        pure $ (Read.fromField Ignored writerField) { Read.fldDefault = Nothing }

findField :: Field -> [(Int, Field)] -> Maybe (Int, Field)
findField w rs =
  let
    byName = find (\x -> fldName (snd x) == fldName w) rs
    allNames fld = Set.fromList (fldName fld : fldAliases fld)
    fNames = allNames w
    sameField = not . Set.null . Set.intersection fNames . allNames
    byAliases = find (sameField . snd) rs
  in byName <|> byAliases

findTypeV :: Schema -> Vector Schema -> Maybe (Int, Schema)
findTypeV schema schemas =
  let tn = typeName schema
      allNames typ = typeName typ : map renderFullname (aliases typ)
  in ((,) <$> id <*> V.unsafeIndex schemas) <$> 
        V.findIndex ((tn `elem`) . allNames) schemas
