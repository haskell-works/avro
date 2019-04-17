{-# LANGUAGE TupleSections #-}
module Data.Avro.Deconflict
  ( deconflict
  , deconflictNoResolve
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
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- | @deconflict writer reader val@ will convert a value that was
-- encoded/decoded with the writer's schema into the form specified by the
-- reader's schema.
--
-- 'deconflict' will attempt resolving 'TypedName' constructors to make sure that
-- they are handled correctly. This has a performance impact.
-- To avoid it use 'deconflictNoResolve' when possible.
deconflict :: Schema        -- ^ Writer schema
           -> Schema        -- ^ Reader schema
           -> T.Value Type
           -> Either String (T.Value Type)
deconflict writerSchema readerSchema =
  deconflictNoResolve (S.expandNamedTypes writerSchema) (S.expandNamedTypes readerSchema)

-- | @deconflict writer reader val@ will convert a value that was
-- encoded/decoded with the writer's schema into the form specified by the
-- reader's schema.
--
-- A faster version of 'deconflict' which does not attempt to resolve 'TypedName' references.
-- It still checks if the referenced type has the same name, but does not traverses these references.
--
-- 'deconflictNoResolve' should typically be used when a number of values are decoded with
-- the same reader and writer schemas. In this case schemas can only be resolved once
-- to be used in 'deconflictNoResolve'.
deconflictNoResolve :: Schema         -- ^ Writer schema
                    -> Schema         -- ^ Reader schema
                    -> T.Value Type
                    -> Either String (T.Value Type)
deconflictNoResolve writerSchema readerSchema =
  deconflictValue writerSchema readerSchema

deconflictValue :: Schema
              -> Schema
              -> T.Value Type
              -> Either String (T.Value Type)
deconflictValue writerSchema readerSchema v
  | writerSchema == readerSchema    = Right v
  | otherwise = go writerSchema readerSchema v
  where
  go :: Type -> Type -> T.Value Type -> Either String (T.Value Type)
  go (S.Array aTy) (S.Array bTy) (T.Array vec) =
       T.Array <$> mapM (go aTy bTy) vec
  go (S.Map aTy) (S.Map bTy) (T.Map mp)    =
       T.Map <$> mapM (go aTy bTy) mp
  go a@S.Enum {} b@S.Enum {} val
       | name a == name b = deconflictEnum a b val
  go a@S.Fixed {} b@S.Fixed {} val
       | name a == name b && size a == size b = Right val
  go a@S.Record {} b@S.Record {} val
       | name a == name b = deconflictRecord a b val
  go (S.Union xs) (S.Union ys) (T.Union _ tyVal val) =
       withSchemaIn tyVal xs $ \sch -> deconflictReaderUnion sch ys val
  go nonUnion (S.Union ys) val =
       deconflictReaderUnion nonUnion ys val
  go (S.Union xs) nonUnion (T.Union _ tyVal val) =
       withSchemaIn tyVal xs $ \sch -> deconflictValue sch nonUnion val
  go eTy dTy val =
    case val of
      T.Int i32  | dTy == S.Long   -> Right $ T.Long   (fromIntegral i32)
                 | dTy == S.Float  -> Right $ T.Float  (fromIntegral i32)
                 | dTy == S.Double -> Right $ T.Double (fromIntegral i32)
      T.Long i64 | dTy == S.Float  -> Right $ T.Float (fromIntegral i64)
                 | dTy == S.Double -> Right $ T.Double (fromIntegral i64)
      T.Float f  | dTy == S.Double -> Right $ T.Double (realToFrac f)
      T.String s | dTy == S.Bytes  -> Right $ T.Bytes (Text.encodeUtf8 s)
      T.Bytes bs | dTy == S.String -> Right $ T.String (Text.decodeUtf8 bs)
      _                            -> Left $ "Can not resolve differing writer and reader schemas: " ++ show (eTy, dTy)

-- The writer's symbol must be present in the reader's enum
deconflictEnum :: Type -> Type -> T.Value Type -> Either String (T.Value Type)
deconflictEnum e d val@(T.Enum _ _ _txt) = Right val
  -- --  | txt `elem` symbols d = Right val
  -- --  | otherwise = Left "Decoded enum does not appear in reader's symbol list."

withSchemaIn :: (Foldable f, Functor f)
  => Schema
  -> f Schema
  -> (Schema -> Either String a)
  -> Either String a
withSchemaIn schema schemas f =
  case findType schema schemas of
    Nothing    -> Left $ "Incorrect payload: union " <> (show . Foldable.toList $ typeName <$> schemas) <> " does not contain schema " <> Text.unpack (typeName schema)
    Just found -> f found

deconflictReaderUnion :: Type -> Vector Type -> T.Value Type -> Either String (T.Value Type)
deconflictReaderUnion valueSchema unionTypes val =
    let hdl [] = Left "Impossible: empty non-empty list."
        hdl (d:rest) =
              case deconflictValue valueSchema d val of
                Right v -> Right (T.Union unionTypes d v)
                Left _  -> hdl rest
    in hdl (V.toList unionTypes)

deconflictRecord :: Type -> Type -> T.Value Type -> Either String (T.Value Type)
deconflictRecord writerSchema readerSchema (T.Record ty fldVals)  =
  T.Record readerSchema . HashMap.fromList <$> mapM (deconflictFields fldVals (fields writerSchema)) (fields readerSchema)

-- For each field of the decoders, lookup the field in the hash map
--  1) If the field exists, call 'deconflictValue'
--  2) If the field is missing use the reader's default
--  3) If there is no default, fail.
--
-- XXX: Consider aliases in the writer schema, use those to retry on failed lookup.
deconflictFields :: HashMap Text (T.Value Type) -> [Field] -> Field -> Either String (Text,T.Value Type)
deconflictFields hm writerFields readerField =
  let
    mbWriterField = findField readerField writerFields
    mbValue = HashMap.lookup (fldName readerField) hm
  in case (mbWriterField, mbValue, fldDefault readerField) of
    (Just w, Just x,_)  -> (fldName readerField,) <$> deconflictValue (fldType w) (fldType readerField) x
    (_, Just x,_)       -> Right (fldName readerField, x)
    (_, _,Just def)     -> Right (fldName readerField, def)
    (_,Nothing,Nothing) -> Left $ "No field and no default for " ++ show (fldName readerField)

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
