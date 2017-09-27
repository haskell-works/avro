{-# LANGUAGE OverloadedStrings     #-}
module Data.Avro.Deriving.NormSchema
where

import           Data.Avro.Schema
import qualified Data.Set as S
import           Data.List.NonEmpty         (NonEmpty( (:|) ))
import           Control.Monad.State.Strict
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Semigroup ((<>))

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
      r@(Record _ _ _ _ _ fs) -> r : (fs >>= (getTypes . fldType))
      Array t                 -> getTypes t
      Union (t1 :| ts) _      -> getTypes t1 <> concatMap getTypes ts
      Map t                   -> getTypes t
      e@Enum{}                -> [e]
      _                       -> []

-- TODO: Currently ensures normalisation: only in one way
-- that is needed for "extractRecord".
-- it ensures that an "extracted" record is self-contained and
-- all the named types are resolvable within the scope of the schema.
-- The other way around (to each record is inlined only once and is referenced
-- as a named type after that) is not implemented.
normSchema :: [Schema] -- ^ List of all possible records
           -> Schema   -- ^ Schema to normalise
           -> State (S.Set TypeName) Schema
normSchema rs r = case r of
  t@(NamedType tn) -> do
    let sn = shortName tn
    resolved <- get
    if S.member sn resolved
      then pure t
      else do
        modify' (S.insert sn)
        pure $ fromMaybe (error $ "Unable to resolve schema: " <> show (typeName t)) (findSchema tn)
  Array s   -> Array <$> normSchema rs s
  Map s     -> Map <$> normSchema rs s
  Record{name = tn}  -> do
    let sn = shortName tn
    modify' (S.insert sn)
    flds <- mapM (\fld -> setType fld <$> normSchema rs (fldType fld)) (fields r)
    pure $ r { fields = flds }
  s         -> pure s
  where
    shortName tn = TN $ T.takeWhileEnd (/='.') (unTN tn)
    setType fld t = fld { fldType = t}
    fullName s = TN $ maybe (typeName s) (\n -> typeName s <> "." <> n) (namespace s)
    findSchema tn = L.find (\s -> name s == tn || fullName s == tn) rs