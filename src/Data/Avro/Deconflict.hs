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

-- | Given an encoder schema and the Avro value it produced,
-- and a desired decoder schema, produce an Avro value in the
-- form expected by the decoder if possible.
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
resolveEnum e d val@(T.Enum txt)
   | txt `elem` symbols d = Right val
   | otherwise = Left "Decoded enum does not appear in reader's symbol list."

resolveTwoUnions :: [Type] -> T.Value Type -> Either String (T.Value Type)
resolveTwoUnions  ds (T.Union eTy val) =
    resolveReaderUnion eTy ds val

resolveReaderUnion :: Type -> [Type] -> T.Value Type -> Either String (T.Value Type)
resolveReaderUnion e ds val =
    let hdl [] = Left "No reader schema's in the unions match the writer's schema."
        hdl (d:rest) =
              case resolveSchema e d val of
                Right val -> Right (T.Union d val)
                Left _    -> hdl rest
    in hdl ds

resolveWriterUnion :: Type -> T.Value Type -> Either String (T.Value Type)
resolveWriterUnion reader (T.Union ty val) = resolveSchema ty reader val

resolveRecord :: DeclaredType -> DeclaredType -> T.Value Type -> Either String (T.Value Type)
resolveRecord eRec dRec (T.Record fldVals)  =
 resolveFields (fields eRec) (fields dRec) fldVals

resolveFields :: [Field] -> [Field] -> HashMap Text (T.Value Type) -> Either String (T.Value Type)
resolveFields (ef:eFlds) (df:dFlds) = undefined
