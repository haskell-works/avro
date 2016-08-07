{-# LANGUAGE TupleSections #-}
module Data.Avro.Deconflict
  ( deconflict
  ) where

import           Data.Avro.Schema    as S
import           Data.Avro.Types     as T
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text

-- | @deconflict writer reader val@ will convert a value that was
-- encoded/decoded with the writer's schema into the form specified by the
-- reader's schema.
deconflict :: Schema -> Schema -> T.Value Type -> Either String (T.Value Type)
deconflict writerType readerType val =
  resolveSchema writerType readerType val

resolveSchema :: Type -> Type -> T.Value Type -> Either String (T.Value Type)
resolveSchema e d v
  | e == d    = Right v
  | otherwise = go e d v
  where
  go :: Type -> Type -> T.Value Type -> Either String (T.Value Type)
  go (S.Array aTy) (S.Array bTy) (T.Array vec) =
       T.Array <$> mapM (go aTy bTy) vec
  go (S.Map aTy) (S.Map bTy) (T.Map mp)    =
       T.Map <$> mapM (go aTy bTy) mp
  go a@(S.Enum {}) b@(S.Enum {}) val
       | name a == name b = resolveEnum a b val
  go a@(S.Fixed {}) b@(S.Fixed {}) val
       | name a == name b && size a == size b = Right val
  go a@(S.Record {}) b@(S.Record {}) val
       | name a == name b = resolveRecord a b val
  go (S.Union _ _) (S.Union ys _) val =
       resolveTwoUnions ys val
  go nonUnion (S.Union ys _) val =
       resolveReaderUnion nonUnion ys val
  go (S.Union _xs _) nonUnion val =
       resolveWriterUnion nonUnion val
  go eTy dTy val =
    case val of
      T.Int i32 | dTy == S.Long    -> Right $ T.Long   (fromIntegral i32)
                | dTy == S.Float   -> Right $ T.Float  (fromIntegral i32)
                | dTy == S.Double  -> Right $ T.Double (fromIntegral i32)
      T.Long i64 | dTy == S.Float  -> Right $ T.Float (fromIntegral i64)
                 | dTy == S.Double -> Right $ T.Double (fromIntegral i64)
      T.Float f | dTy == S.Double  -> Right $ T.Double (realToFrac f)
      T.String s | dTy == S.Bytes  -> Right $ T.Bytes (Text.encodeUtf8 s)
      T.Bytes bs | dTy == S.String -> Right $ T.String (Text.decodeUtf8 bs)
      _                            -> Left $ "Can not resolve differing writer and reader schemas: " ++ show (eTy, dTy)

-- The writer's symbol must be present in the reader's enum
resolveEnum :: Type -> Type -> T.Value Type -> Either String (T.Value Type)
resolveEnum e d val@(T.Enum _ _ _txt) = Right val
   -- | txt `elem` symbols d = Right val
   -- | otherwise = Left "Decoded enum does not appear in reader's symbol list."

resolveTwoUnions :: NonEmpty Type -> T.Value Type -> Either String (T.Value Type)
resolveTwoUnions  ds (T.Union _ eTy val) =
    resolveReaderUnion eTy ds val

resolveReaderUnion :: Type -> NonEmpty Type -> T.Value Type -> Either String (T.Value Type)
resolveReaderUnion e ds val =
    let hdl [] = Left "Impossible: empty non-empty list."
        hdl (d:rest) =
              case resolveSchema e d val of
                Right v   -> Right (T.Union ds d v)
                Left _    -> hdl rest
    in hdl (NE.toList ds)

resolveWriterUnion :: Type -> T.Value Type -> Either String (T.Value Type)
resolveWriterUnion reader (T.Union _ ty val) = resolveSchema ty reader val

resolveRecord :: Type -> Type -> T.Value Type -> Either String (T.Value Type)
resolveRecord eRec dRec (T.Record fldVals)  =
 T.Record . HashMap.fromList <$> mapM (resolveFields fldVals (fields eRec)) (fields dRec)

-- For each field of the decoders, lookup the field in the hash map
--  1) If the field exists, call 'resolveSchema'
--  2) If the field is missing use the reader's default
--  3) If there is no default, fail.
--
-- XXX: Consider aliases in the writer schema, use those to retry on failed lookup.
resolveFields :: HashMap Text (T.Value Type) -> [Field] -> Field -> Either String (Text,T.Value Type)
resolveFields hm eFlds d =
  case (HashMap.lookup (fldName d) hm, fldDefault d) of
    (Just x,_)        -> Right (fldName d, x)
    (_,Just def)      -> Right (fldName d,def)
    (Nothing,Nothing) -> Left $ "No field and no default for " ++ show (fldName d)
