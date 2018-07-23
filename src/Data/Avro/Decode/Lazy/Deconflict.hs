module Data.Avro.Decode.Lazy.Deconflict
  ( deconflict
  ) where

import           Control.Applicative             ((<|>))
import           Data.Avro.Decode.Lazy.Convert   (fromStrictValue)
import           Data.Avro.Decode.Lazy.LazyValue as T
import           Data.Avro.Schema                as S
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

-- | @deconflict writer reader val@ will convert a value that was
-- encoded/decoded with the writer's schema into the form specified by the
-- reader's schema.
deconflict :: Schema        -- ^ Writer schema
           -> Schema        -- ^ Reader schema
           -> T.LazyValue Type
           -> T.LazyValue Type
deconflict = resolveSchema

resolveSchema :: Type -> Type -> T.LazyValue Type -> T.LazyValue Type
resolveSchema writerSchema readerSchema v
  | writerSchema == readerSchema    = v
  | otherwise = go writerSchema readerSchema v
  where
    go :: Type -> Type -> T.LazyValue Type -> T.LazyValue Type
    go _ _ val@(T.Error _) = val
    go (S.Array aTy) (S.Array bTy) (T.Array vec) =
        T.Array $ fmap (go aTy bTy) vec
    go (S.Map aTy) (S.Map bTy) (T.Map mp)    =
        T.Map $ fmap (go aTy bTy) mp
    go a@S.Enum {} b@S.Enum {} val
        | name a == name b = resolveEnum a b val
    go a@S.Fixed {} b@S.Fixed {} val
        | name a == name b && size a == size b = val
    go a@S.Record {} b@S.Record {} val
        | name a == name b = resolveRecord a b val
    go (S.Union _ _) (S.Union ys _) val =
        resolveTwoUnions ys val
    go nonUnion (S.Union ys _) val =
        resolveReaderUnion nonUnion ys val
    go (S.Union _xs _) nonUnion val =
        resolveWriterUnion nonUnion val
    go eTy dTy val =
      case val of
        T.Int i32 | dTy == S.Long    -> T.Long   (fromIntegral i32)
                  | dTy == S.Float   -> T.Float  (fromIntegral i32)
                  | dTy == S.Double  -> T.Double (fromIntegral i32)
        T.Long i64 | dTy == S.Float  -> T.Float (fromIntegral i64)
                  | dTy == S.Double -> T.Double (fromIntegral i64)
        T.Float f | dTy == S.Double  -> T.Double (realToFrac f)
        T.String s | dTy == S.Bytes  -> T.Bytes (Text.encodeUtf8 s)
        T.Bytes bs | dTy == S.String -> T.String (Text.decodeUtf8 bs)
        _                            -> T.Error $ "Can not resolve differing writer and reader schemas: " ++ show (eTy, dTy)

-- The writer's symbol must be present in the reader's enum
resolveEnum :: Type -> Type -> T.LazyValue Type -> T.LazyValue Type
resolveEnum e d val@(T.Enum _ _ _txt) = val
  -- --  | txt `elem` symbols d = Right val
  -- --  | otherwise = Left "Decoded enum does not appear in reader's symbol list."

resolveTwoUnions :: NonEmpty Type -> T.LazyValue Type -> T.LazyValue Type
resolveTwoUnions ds (T.Union _ eTy val) =
  resolveReaderUnion eTy ds val

resolveReaderUnion :: Type -> NonEmpty Type -> T.LazyValue Type -> T.LazyValue Type
resolveReaderUnion e ds val =
  let hdl [] = T.Error $ "No corresponding union value for " <> Text.unpack (typeName e)
      hdl (d:rest) =
            case resolveSchema e d val of
              T.Error _ -> hdl rest
                -- Right (T.Union ds d v)
              v         -> T.Union ds d v
  in hdl (NE.toList ds)

resolveWriterUnion :: Type -> T.LazyValue Type -> T.LazyValue Type
resolveWriterUnion reader (T.Union _ ty val) = resolveSchema ty reader val

resolveRecord :: Type -> Type -> T.LazyValue Type -> T.LazyValue Type
resolveRecord writerSchema readerSchema (T.Record ty fldVals)  =
  T.Record ty . HashMap.fromList $ fmap (resolveFields fldVals (fields writerSchema)) (fields readerSchema)

-- For each field of the decoders, lookup the field in the hash map
--  1) If the field exists, call 'resolveSchema'
--  2) If the field is missing use the reader's default
--  3) If there is no default, fail.
--
-- XXX: Consider aliases in the writer schema, use those to retry on failed lookup.
resolveFields :: HashMap Text (T.LazyValue Type) -> [Field] -> Field -> (Text,T.LazyValue Type)
resolveFields hm writerFields readerField =
  let
    mbWriterField = findField readerField writerFields
    mbValue = HashMap.lookup (fldName readerField) hm
  in case (mbWriterField, mbValue, fldDefault readerField) of
    (Just w, Just x,_)   -> (fldName readerField, resolveSchema (fldType w) (fldType readerField) x)
    (_, Just x,_)  -> (fldName readerField, x)
    (_, _,Just def)      -> (fldName readerField, fromStrictValue def)
    (_,Nothing,Nothing)  -> (fldName readerField, T.Error ("No field and no default for " ++ show (fldName readerField)))

findField :: Field -> [Field] -> Maybe Field
findField f fs =
  let
    byName = find (\x -> fldName x == fldName f) fs
    allNames fld = Set.fromList (fldName fld : fldAliases fld)
    fNames = allNames f
    sameField = not . Set.null . Set.intersection fNames . allNames
    byAliases = find sameField fs
  in byName <|> byAliases
