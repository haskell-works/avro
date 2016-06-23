{-# LANGUAGE TupleSections #-}
module Data.Avro.Deconflict
  ( deconflict
  ) where

import Data.Avro.Schema as S
import Data.Avro.Types as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | @deconflict writer reader val@ will convert a value that was
-- encoded/decoded with the writer's schema into the form specified by the
-- reader's schema.
deconflict :: Schema -> Schema -> T.Value Type -> Either String (T.Value Type)
deconflict (Schema writerType) (Schema readerType) val =
  resolveSchema writerType readerType val

resolveSchema :: Type -> Type -> T.Value Type -> Either String (T.Value Type)
resolveSchema e d v
  | e == d    = Right v
  | otherwise = go e d v
  where
  go :: Type -> Type -> T.Value Type -> Either String (T.Value Type)
  go (BasicType (S.Array aTy)) (BasicType (S.Array bTy)) (T.Array vec) =
       T.Array <$> mapM (go aTy bTy) vec
  go (BasicType (S.Map aTy)) (BasicType (S.Map bTy)) (T.Map mp)    =
       T.Map <$> mapM (go aTy bTy) mp
  go (BasicType eTy) (BasicType dTy) val
       = case val of
            T.Int i32 | dTy == S.Long    -> Right $ T.Long   (fromIntegral i32)
                      | dTy == S.Float   -> Right $ T.Float  (fromIntegral i32)
                      | dTy == S.Double  -> Right $ T.Double (fromIntegral i32)
            T.Long i64 | dTy == S.Float  -> Right $ T.Float (fromIntegral i64)
                       | dTy == S.Double -> Right $ T.Double (fromIntegral i64)
            T.Float f | dTy == S.Double  -> Right $ T.Double (realToFrac f)
            T.String s | dTy == S.Bytes  -> Right $ T.Bytes (Text.encodeUtf8 s)
            T.Bytes bs | dTy == S.String -> Right $ T.String (Text.decodeUtf8 bs)
            _                            -> Left $ "Basic types do not match: " ++ show (eTy, dTy)
 
  go (DeclaredType a@(S.Enum {})) (DeclaredType b@(S.Enum {})) val
       | name a == name b = resolveEnum a b val
  go (DeclaredType a@(S.Fixed {})) (DeclaredType b@(S.Fixed {})) val
       | name a == name b && size a == size b = Right val
  go (DeclaredType a@(S.Record {})) (DeclaredType b@(S.Record {})) val
       | name a == name b = resolveRecord a b val
  go (DeclaredType (S.Union _)) (DeclaredType (S.Union ys)) val =
       resolveTwoUnions ys val
  go nonUnion (DeclaredType (S.Union ys)) val =
       resolveReaderUnion nonUnion ys val
  go (DeclaredType (S.Union xs)) nonUnion val =
       resolveWriterUnion nonUnion val
  go x y _ = Left $ "Can not resolve differing writer and reader schemas: " ++ show (x,y)

-- The writer's symbol must be present in the reader's enum
resolveEnum :: DeclaredType -> DeclaredType -> T.Value Type -> Either String (T.Value Type)
resolveEnum e d val@(T.Enum _ txt)
   | txt `elem` symbols d = Right val
   | otherwise = Left "Decoded enum does not appear in reader's symbol list."

resolveTwoUnions :: [Type] -> T.Value Type -> Either String (T.Value Type)
resolveTwoUnions  ds (T.Union _ eTy val) =
    resolveReaderUnion eTy ds val

resolveReaderUnion :: Type -> [Type] -> T.Value Type -> Either String (T.Value Type)
resolveReaderUnion e ds val =
    let hdl [] = Left "No reader schema's in the unions match the writer's schema."
        hdl (d:rest) =
              case resolveSchema e d val of
                Right v   -> Right (T.Union ds d v)
                Left _    -> hdl rest
    in hdl ds

resolveWriterUnion :: Type -> T.Value Type -> Either String (T.Value Type)
resolveWriterUnion reader (T.Union _ ty val) = resolveSchema ty reader val

resolveRecord :: DeclaredType -> DeclaredType -> T.Value Type -> Either String (T.Value Type)
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
