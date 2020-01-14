module Data.Avro.Decode.Lazy.Deconflict
  ( deconflict
  , deconflictNoResolve
  ) where

import           Control.Applicative             ((<|>))
import           Data.Avro.Decode.Lazy.Convert   (fromStrictValue)
import           Data.Avro.Decode.Lazy.LazyValue as T
import           Data.Avro.Schema                as S
import qualified Data.Foldable                   as Foldable
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.List                       (find)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map                        as M
import           Data.Semigroup                  ((<>))
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V

{-# DEPRECATED deconflict, deconflictNoResolve "Use Data.Avro.Schema.Deconflict.deconflict or Data.Avro.Decode.Lazy.decodeContainerWithSchema instead." #-}

type Deconflicter =
     Schema        -- ^ Writer schema
  -> Schema        -- ^ Reader schema
  -> T.LazyValue Schema
  -> T.LazyValue Schema

-- | @deconflict writer reader val@ will convert a value that was
-- encoded/decoded with the writer's schema into the form specified by the
-- reader's schema.
--
-- 'deconflict' will attempt resolving 'TypedName' constructors to make sure that
-- they are handled correctly. This has a performance impact.
-- To avoid it use 'deconflictNoResolve' when possible.
deconflict :: Schema        -- ^ Writer schema
           -> Schema        -- ^ Reader schema
           -> T.LazyValue Schema
           -> T.LazyValue Schema
deconflict = startDeconflict True

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
                    -> T.LazyValue Schema
                    -> T.LazyValue Schema
deconflictNoResolve = startDeconflict False

startDeconflict :: Bool -> Deconflicter
startDeconflict shouldExpandNames writerSchema readerSchema = go writerSchema readerSchema
  where
    aEnv = S.buildTypeEnvironment (const $ Left "Bad Schema") writerSchema
    bEnv = S.buildTypeEnvironment (const $ Left "Bad Schema") readerSchema
    go :: Deconflicter
    go aTy bTy val
        | not shouldExpandNames && aTy == bTy = val
    go _ _ val@(T.Error _) = val
    go (S.Array aTy) (S.Array bTy) (T.Array vec) =
        T.Array $ fmap (go aTy bTy) vec
    go (S.Map aTy) (S.Map bTy) (T.Map mp)    =
        T.Map $ fmap (go aTy bTy) mp
    go a@S.Enum {} b@S.Enum {} val
        | name a == name b = deconflictEnum a b val
    go a@S.Fixed {} b@S.Fixed {} val
        | name a == name b && size a == size b = val
    go a@S.Record {} b@S.Record {} val
        | name a == name b = deconflictRecord go a b val
    go (S.Union xs) (S.Union ys) (T.Union _ tyVal val) =
        withSchemaIn tyVal xs $ \sch -> deconflictReaderUnion go sch (extractValues ys) val
    go nonUnion (S.Union ys) val =
        deconflictReaderUnion go nonUnion (extractValues ys) val
    go (S.Union xs) nonUnion (T.Union _ tyVal val) =
        withSchemaIn tyVal xs $ \sch -> go sch nonUnion val
    go (S.NamedType t) bTy val =
      either T.Error (\aTy -> go aTy bTy val) $ aEnv t
    go aTy (S.NamedType t) val =
      either T.Error (\bTy -> go aTy bTy val) $ bEnv t
    go aTy bTy val | aTy == bTy = val
    go eTy dTy val =
      case val of
        T.Int s v     | S.Long _ <- dTy   -> T.Long    s (fromIntegral v)
                      | dTy == S.Float    -> T.Float   s (fromIntegral v)
                      | dTy == S.Double   -> T.Double  s (fromIntegral v)
        T.Long s v    | dTy == S.Float    -> T.Float   s (fromIntegral v)
                      | dTy == S.Double   -> T.Double  s (fromIntegral v)
        T.Float s v   | dTy == S.Double   -> T.Double  s (realToFrac v)
        T.String s v  | S.Bytes _ <- dTy  -> T.Bytes   s (Text.encodeUtf8 v)
        T.Bytes s v   | S.String _ <- dTy -> T.String  s (Text.decodeUtf8 v)
        _                              -> T.Error $ "Can not resolve differing writer and reader schemas: " ++ show (eTy, dTy)

-- The writer's symbol must be present in the reader's enum
deconflictEnum :: Schema -> Schema -> T.LazyValue Schema -> T.LazyValue Schema
deconflictEnum e d val@(T.Enum _ _ _txt) = val
  -- --  | txt `elem` symbols d = Right val
  -- --  | otherwise = Left "Decoded enum does not appear in reader's symbol list."

withSchemaIn :: (Foldable f, Functor f)
  => Schema
  -> f Schema
  -> (Schema -> LazyValue Schema)
  -> LazyValue Schema
withSchemaIn schema schemas f =
  case findType schema schemas of
    Nothing    -> T.Error $ "Incorrect payload: union " <> (show . Foldable.toList $ typeName <$> schemas) <> " does not contain schema " <> Text.unpack (typeName schema)
    Just found -> f found

deconflictReaderUnion :: Deconflicter -> Schema -> Vector Schema -> T.LazyValue Schema -> T.LazyValue Schema
deconflictReaderUnion go valueType unionTypes val =
  let hdl [] = T.Error $ "No corresponding union value for " <> Text.unpack (typeName valueType)
      hdl (d:rest) =
            case go valueType d val of
              T.Error _ -> hdl rest
              v         -> T.Union unionTypes d v
  in hdl (V.toList unionTypes)

deconflictRecord :: Deconflicter -> Schema -> Schema -> T.LazyValue Schema -> T.LazyValue Schema
deconflictRecord go writerSchema readerSchema (T.Record ty fldVals)  =
  T.Record readerSchema . HashMap.fromList $ fmap (deconflictFields go fldVals (fields writerSchema)) (fields readerSchema)

-- For each field of the decoders, lookup the field in the hash map
--  1) If the field exists, call the given deconflicting function
--  2) If the field is missing use the reader's default
--  3) If there is no default, fail.
--
-- XXX: Consider aliases in the writer schema, use those to retry on failed lookup.
deconflictFields :: Deconflicter -> HashMap Text (T.LazyValue Schema) -> [Field] -> Field -> (Text,T.LazyValue Schema)
deconflictFields go hm writerFields readerField =
  let
    mbWriterField = findField readerField writerFields
    mbValue = HashMap.lookup (fldName readerField) hm
  in case (mbWriterField, mbValue, fldDefault readerField) of
    (Just w, Just x,_)  -> (fldName readerField, go (fldType w) (fldType readerField) x)
    (_, Just x,_)       -> (fldName readerField, x)
    (_, _,Just def)     -> (fldName readerField, fromStrictValue def)
    (_,Nothing,Nothing) -> (fldName readerField, T.Error ("No field and no default for " ++ show (fldName readerField)))

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
