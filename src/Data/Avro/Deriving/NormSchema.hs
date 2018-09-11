{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Avro.Deriving.NormSchema
where

import           Control.Monad.State.Strict
import           Data.Avro.Schema
import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T

-- | Extracts all the records from the schema (flattens the schema)
-- Named types get resolved when needed to include at least one "inlined"
-- schema in each record and to make each record self-contained.
-- Note: Namespaces are not really supported in this version. All the
-- namespaces (including inlined into full names) will be ignored
-- during names resolution.
extractDerivables :: Schema -> [Schema]
extractDerivables s = flip evalState S.empty . normSchema rawRecs <$> rawRecs
  where
    rawRecs = getTypes s
    getTypes rec = case rec of
      r@(Record _ _ _ _ fs) -> r : (fs >>= (getTypes . fldType))
      Array t               -> getTypes t
      Union (t1 :| ts) _    -> getTypes t1 <> concatMap getTypes ts
      Map t                 -> getTypes t
      e@Enum{}              -> [e]
      f@Fixed{}             -> [f]
      _                     -> []

-- TODO: Currently ensures normalisation: only in one way
-- that is needed for "extractRecord".
-- it ensures that an "extracted" record is self-contained and
-- all the named types are resolvable within the scope of the schema.
-- The other way around (to each record is inlined only once and is referenced
-- as a named type after that) is not implemented.
normSchema :: [Schema] -- ^ List of all possible records
           -> Schema   -- ^ Schema to normalise
           -> State (S.Set TypeName) Schema
normSchema schemas = \case
  t@(NamedType name) -> do
    resolved <- get
    if S.member name resolved
      then pure t
      else do
        modify' $ S.insert name
        let unresolved = error $ "Unable to resolve schema: " <> show (typeName t)
        pure $ fromMaybe unresolved $ findSchema name
  Array s           -> Array <$> normSchema schemas s
  Map s             -> Map <$> normSchema schemas s
  r@Record { name } -> do
    modify' $ S.insert name
    flds <- mapM (\fld -> setType fld <$> normSchema schemas (fldType fld)) (fields r)
    pure $ r { fields = flds }
  s         -> pure s
  where setType fld t = fld { fldType = t}
        findSchema tn = L.find (\ s -> name s == tn) schemas
