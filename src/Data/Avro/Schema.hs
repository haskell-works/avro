{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Avro 'Schema's, represented here as values of type 'Schema',
-- describe the serialization and de-serialization of values.
--
-- In Avro schemas are compose-able such that encoding data under a schema and
-- decoding with a variant, such as newer or older version of the original
-- schema, can be accomplished by using the 'Data.Avro.Deconflict' module.
module Data.Avro.Schema
  (
   -- * Schema description types
    Schema, Type(..)
  , Field(..), Order(..)
  , TypeName(..)
  , mkEnum, mkUnion
  , validateSchema
  -- * Lower level utilities
  , typeName
  , buildTypeEnvironment
  , Result(..)
  ) where

import           Prelude as P
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Control.Monad.Fail as MF
import qualified Data.Aeson as A
import           Data.Aeson ((.=),object,(.:?),(.:),(.!=),FromJSON(..),ToJSON(..))
import           Data.Aeson.Types (Parser,typeMismatch)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Monoid ((<>), First(..))
import qualified Data.Set as S
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Avro.Types as Ty
import qualified Data.IntMap as IM
import Data.Int
import Text.Show.Functions

-- |An Avro schema is either
-- * A "JSON object in the form `{"type":"typeName" ...`
-- * A "JSON string, naming a defined type" (basic type w/o free variables/names)
-- * A "JSON array, representing a union"
--
-- N.B. It is possible to create a Haskell value (of Schema type) that is
-- not a valid Avro schema by violating one of the above or one of the
-- conditions called out in 'validateSchema'.
type Schema = Type

-- |Avro types are considered either primitive (string, int, etc) or
-- complex/declared (structures, unions etc).
data Type
      =
      -- Basic types
        Null
      | Boolean
      | Int   | Long
      | Float | Double
      | Bytes | String
      | Array { item :: Type }
      | Map   { values :: Type }
      | NamedType TypeName
      -- Declared types
      | Record { name      :: TypeName
               , namespace :: Maybe Text
               , aliases   :: [TypeName]
               , doc       :: Maybe Text
               , order     :: Maybe Order
               , fields    :: [Field]
               }
      | Enum { name      :: TypeName
             , namespace :: Maybe Text
             , aliases   :: [TypeName]
             , doc       :: Maybe Text
             , symbols   :: [Text]
             , symbolLookup :: Int64 -> Maybe Text
             }
      | Union { options  :: NonEmpty Type
              , unionLookup :: Int64 -> Maybe Type
              }
      | Fixed { name        :: TypeName
              , namespace   :: Maybe Text
              , aliases     :: [TypeName]
              , size        :: Integer
              }
    deriving (Show)

instance Eq Type where
  Null == Null = True
  Boolean == Boolean = True
  Int   == Int = True
  Long == Long = True
  Float == Float = True
  Double == Double = True
  Bytes == Bytes = True
  String == String = True
  Array ty == Array ty2 = ty == ty2
  Map ty == Map ty2 = ty == ty2
  NamedType t == NamedType t2 = t == t2
  Record name1 ns1 _ _ _ fs1 == Record name2 ns2 _ _ _ fs2 =
    and [name1 == name2, ns1 == ns2, fs1 == fs2]
  Enum _ _ _ _ s _ == Enum _ _ _ _ s2 _ = s == s2
  Union a _ == Union b _ = a == b
  Fixed _ _ _ s == Fixed _ _ _ s2 = s == s2
  _ == _ = False

-- | @mkEnum name aliases namespace docs syms@ Constructs an `Enum` schema using
-- the enumeration type's name, aliases (if any), namespace, documentation, and list of
-- symbols that inhabit the enumeration.
mkEnum :: TypeName -> [TypeName] -> Maybe Text -> Maybe Text -> [Text] -> Type
mkEnum n as ns d ss = Enum n ns as d ss (\i -> IM.lookup (fromIntegral i) mp)
 where
 mp = IM.fromList (zip [0..] ss)

-- | @mkUnion subTypes@ Defines a union of the provided subTypes.  N.B. it is
-- invalid Avro to include another union or to have more than one of the same
-- type as a direct member of the union.  No check is done for this condition!
mkUnion :: NonEmpty Type -> Type
mkUnion os = Union os (\i -> IM.lookup (fromIntegral i) mp)
 where mp = IM.fromList (zip [0..] $ NE.toList os)

newtype TypeName = TN { unTN :: T.Text }
  deriving (Eq, Ord)

instance Show TypeName where
  show (TN s) = show s

instance Monoid TypeName where
  mempty = TN mempty
  mappend (TN a) (TN b) = TN (a <> b)

instance IsString TypeName where
  fromString = TN . fromString

instance Hashable TypeName where
  hashWithSalt s (TN t) = hashWithSalt (hashWithSalt s ("AvroTypeName" :: Text)) t

-- |Get the name of the type.  In the case of unions, get the name of the
-- first value in the union schema.
typeName :: Type -> Text
typeName bt =
  case bt of
    Null             -> "null"
    Boolean          -> "boolean"
    Int              -> "int"
    Long             -> "long"
    Float            -> "float"
    Double           -> "double"
    Bytes            -> "bytes"
    String           -> "string"
    Array _          -> "array"
    Map   _          -> "map"
    NamedType (TN t) -> t
    Union (x:|_) _   -> typeName x
    _                -> unTN $ name bt

data Field = Field { fldName       :: Text
                   , fldAliases    :: [Text]
                   , fldDoc        :: Maybe Text
                   , fldOrder      :: Maybe Order
                   , fldType       :: Type
                   , fldDefault    :: Maybe (Ty.Value Type)
                   }
  deriving (Eq, Show)

data Order = Ascending | Descending | Ignore
  deriving (Eq, Ord, Show)

instance FromJSON Type where
  parseJSON (A.String s) =
    case s of
      "null"     -> return Null
      "boolean"  -> return Boolean
      "int"      -> return Int
      "long"     -> return Long
      "float"    -> return Float
      "double"   -> return Double
      "bytes"    -> return Bytes
      "string"   -> return String
      somename   -> return (NamedType (TN somename))
  parseJSON (A.Object o) =
    do ty <- o .: ("type" :: Text)
       case ty of
        "map"    -> Map   <$> o .: ("values" :: Text)
        "array"  -> Array <$> o .: ("items"  :: Text)
        "record" ->
          Record <$> o .:  ("name" :: Text)
                 <*> o .:? ("namespace" :: Text)
                 <*> o .:? ("aliases" :: Text) .!= []
                 <*> o .:? ("doc" :: Text)
                 <*> o .:? ("order" :: Text) .!= Just Ascending
                 <*> o .:  ("fields" :: Text)
        "enum"   ->
          mkEnum <$> o .:  ("name" :: Text)
                 <*> o .:? ("aliases" :: Text)  .!= []
                 <*> o .:? ("namespace" :: Text)
                 <*> o .:? ("doc" :: Text)
                 <*> o .:  ("symbols" :: Text)
        "fixed"  ->
           Fixed <$> o .:  ("name" :: Text)
                 <*> o .:? ("namespace" :: Text)
                 <*> o .:? ("aliases" :: Text) .!= []
                 <*> o .:  ("size" :: Text)
        s  -> fail $ "Unrecognized object type: " <> s
  parseJSON (A.Array arr) | V.length arr > 0 =
           mkUnion . NE.fromList <$> mapM parseJSON (V.toList arr)
  parseJSON foo = typeMismatch "Invalid JSON for Avro Schema" foo

instance ToJSON Type where
  toJSON bt =
    case bt of
      Null     -> A.String "null"
      Boolean  -> A.String "boolean"
      Int      -> A.String "int"
      Long     -> A.String "long"
      Float    -> A.String "float"
      Double   -> A.String "double"
      Bytes    -> A.String "bytes"
      String   -> A.String "string"
      Array tn -> object [ "type" .= ("array" :: Text), "items" .= tn ]
      Map tn   -> object [ "type" .= ("map" :: Text), "values" .= tn ]
      NamedType (TN tn) -> A.String tn
      Record {..} ->
        let opts = catMaybes
               [ ("order" .=)     <$> order
               , ("namespace" .=) <$> namespace
               , ("doc" .=)       <$> doc
               ]
         in object $ opts ++
               [ "type"      .= ("record" :: Text)
               , "name"      .= name
               , "aliases"   .= aliases
               , "fields"    .= fields
               ]
      Enum   {..} ->
        let opts = catMaybes
               [ ("namespace" .=) <$> namespace
               , ("doc" .=)       <$> doc
               ]
         in object $ opts ++
               [ "type"      .= ("enum" :: Text)
               , "name"      .= name
               , "aliases"   .= aliases
               , "symbols"   .= symbols
               ]
      Union  {..} -> A.Array $ V.fromList $ P.map toJSON (NE.toList options)
      Fixed  {..} ->
        let opts = catMaybes
               [ ("namespace" .=) <$> namespace ]
         in object $ opts ++
               [ "type"      .= ("fixed" :: Text)
               , "name"      .= name
               , "aliases"   .= aliases
               , "size"      .= size
               ]

instance ToJSON TypeName where
  toJSON (TN t) = A.String t

instance FromJSON TypeName where
  parseJSON (A.String s) = return (TN s)
  parseJSON j = typeMismatch "TypeName" j

instance FromJSON Field where
  parseJSON (A.Object o) =
    do nm  <- o .: "name"
       doc <- o .:? "doc"
       ty  <- o .: "type"
       let err = fail "Haskell Avro bindings does not support default for aliased or recursive types at this time."
       defM <- o .:? "default"
       def <- case parseAvroJSON err ty <$> defM of
                Just (Success x) -> return (Just x)
                Just (Error e)   -> fail e
                Nothing          -> return Nothing
       od  <- o .:? ("order" :: Text)    .!= Just Ascending
       al  <- o .:? ("aliases" :: Text)  .!= []
       return $ Field nm al doc od ty def

  parseJSON j = typeMismatch "Field " j

instance ToJSON Field where
  toJSON Field {..} =
    let opts = catMaybes
           [ ("order" .=)     <$> fldOrder
           , ("doc" .=)       <$> fldDoc
           , ("default" .=)   <$> fldDefault
           ]
     in object $ opts ++
           [ "name"    .= fldName
           , "type"    .= fldType
           , "aliases" .= fldAliases
           ]

instance ToJSON (Ty.Value Type) where
  toJSON av =
    case av of
      Ty.Null              -> A.Null
      Ty.Boolean b         -> A.Bool b
      Ty.Int i             -> A.Number (fromIntegral i)
      Ty.Long i            -> A.Number (fromIntegral i)
      Ty.Float f           -> A.Number (realToFrac f)
      Ty.Double d          -> A.Number (realToFrac d)
      Ty.Bytes bs          -> A.String ("\\u" <> T.decodeUtf8 (Base16.encode bs))
      Ty.String t          -> A.String t
      Ty.Array vec         -> A.Array (V.map toJSON vec)
      Ty.Map mp            -> A.Object (HashMap.map toJSON mp)
      Ty.Record _ flds     -> A.Object (HashMap.map toJSON flds)
      Ty.Union _ _ Ty.Null -> A.Null
      Ty.Union _ ty val    -> object [ typeName ty .= val ]
      Ty.Fixed bs          -> A.String ("\\u" <> T.decodeUtf8 (Base16.encode bs))  -- XXX the example wasn't literal - this should be an actual bytestring... somehow.
      Ty.Enum _ _ txt      -> A.String txt

data Result a = Success a | Error String
  deriving (Eq,Ord,Show)

instance Monad Result where
  return = pure
  Success a >>= k = k a
  Error e >>= _ = Error e
  fail = MF.fail
instance Functor Result where
  fmap f (Success x) = Success (f x)
  fmap _ (Error e)   = Error e
instance MF.MonadFail Result where
  fail = Error
instance MonadError String Result where
  throwError = fail
  catchError a@(Success _) _ = a
  catchError (Error e) k     = k e
instance Applicative Result where
  pure  = Success
  (<*>) = ap
instance Alternative Result where
  empty = mzero
  (<|>) = mplus
instance MonadPlus Result where
  mzero = fail "mzero"
  mplus a@(Success _) _ = a
  mplus _ b = b
instance Monoid (Result a) where
  mempty = fail "Empty Result"
  mappend = mplus
instance Foldable Result where
  foldMap _ (Error _)   = mempty
  foldMap f (Success y) = f y
  foldr _ z (Error _)   = z
  foldr f z (Success y) = f y z
instance Traversable Result where
  traverse _ (Error err) = pure (Error err)
  traverse f (Success v) = Success <$> f v

-- |Parse JSON-encoded avro data.
parseAvroJSON :: (Text -> Maybe Type) -> Type -> A.Value -> Result (Ty.Value Type)
parseAvroJSON env (NamedType (TN tn)) av =
  case env tn of
    Nothing -> fail $ "Could not resolve type name for " <> show tn
    Just t  -> parseAvroJSON env t av
parseAvroJSON env ty av =
    case av of
      A.String s     ->
        case ty of
          String    -> return $ Ty.String s
          Enum {..} ->
              if s `elem` symbols
                then return $ Ty.Enum ty (maybe (error "IMPOSSIBLE BUG") id $ lookup s (zip symbols [0..])) s
                else fail $ "JSON string is not one of the expected symbols for enum '" <> show name <> "': " <> T.unpack s
          Union tys _ -> do
            f <- tryAllTypes env tys av
            maybe (fail $ "No match for String in union '" <> show (typeName ty) <> "'.") pure f
          _ -> avroTypeMismatch ty "string"
      A.Bool b       -> case ty of
                          Boolean -> return $ Ty.Boolean b
                          _       -> avroTypeMismatch ty "boolean"
      A.Number i     ->
        case ty of
          Int    -> return $ Ty.Int    (floor i)
          Long   -> return $ Ty.Long   (floor i)
          Float  -> return $ Ty.Float  (realToFrac i)
          Double -> return $ Ty.Double (realToFrac i)
          Union tys _ -> do
            f <- tryAllTypes env tys av
            maybe (fail $ "No match for Number in union '" <> show (typeName ty) <> "'.") pure f
          _                   -> avroTypeMismatch ty "number"
      A.Array vec    ->
        case ty of
          Array t -> Ty.Array <$> V.mapM (parseAvroJSON env t) vec
          Union tys _ -> do
            f <- tryAllTypes env tys av
            maybe (fail $ "No match for Array in union '" <> show (typeName ty) <> "'.") pure f
          _  -> avroTypeMismatch ty "array"
      A.Object obj ->
        case ty of
          Map mTy     -> Ty.Map <$> mapM (parseAvroJSON env mTy) obj
          Record {..} ->
           do let lkAndParse f =
                    case HashMap.lookup (fldName f) obj of
                      Nothing -> case fldDefault f of
                                  Just v  -> return v
                                  Nothing -> fail $ "Decode failure: No record field '" <> T.unpack (fldName f) <> "' and no default in schema."
                      Just v  -> parseAvroJSON env (fldType f) v
              Ty.Record ty . HashMap.fromList <$> mapM (\f -> (fldName f,) <$> lkAndParse f) fields
          Union tys _ -> do
            f <- tryAllTypes env tys av
            maybe (fail $ "No match for given record in union '" <> show (typeName ty) <> "'.") pure f
          _ -> avroTypeMismatch ty "object"
      A.Null -> case ty of
                  Null -> return Ty.Null
                  Union us _ | Null `elem` NE.toList us -> return $ Ty.Union us Null Ty.Null
                  _ -> avroTypeMismatch ty "null"

tryAllTypes :: (Text -> Maybe Type) -> NonEmpty Type -> A.Value -> Result (Maybe (Ty.Value Type))
tryAllTypes env tys av =
     getFirst <$> foldMap (\t -> First . Just <$> parseAvroJSON env t av) (NE.toList tys)
                          `catchError` (\_ -> return mempty)

avroTypeMismatch :: Type -> Text -> Result a
avroTypeMismatch expected actual =
  fail $ "Could not resolve type '" <> T.unpack actual <> "' with expected type: " <> show expected

instance ToJSON Order where
  toJSON o =
    case o of
      Ascending  -> A.String "ascending"
      Descending -> A.String "descending"
      Ignore     -> A.String "ignore"

instance FromJSON Order where
  parseJSON (A.String s) =
    case s of
      "ascending"  -> return Ascending
      "descending" -> return Descending
      "ignore"     -> return Ignore
      _            -> fail $ "Unknown string for order: " <> T.unpack s
  parseJSON j = typeMismatch "Order" j

-- | Placeholder NO-OP function!
--
-- Validates a schema to ensure:
--
--  * All types are defined
--  * Unions do not directly contain other unions
--  * Unions are not ambiguous (may not contain more than one schema with
--  the same type except for named types of record, fixed and enum)
--  * Default values for unions can be cast as the type indicated by the
--  first structure.
--  * Default values can be cast/de-serialize correctly.
--  * Named types are resolvable
validateSchema :: Schema -> Parser ()
validateSchema _sch = return () -- XXX TODO

-- | @buildTypeEnvironment schema@ builds a function mapping type names to
-- the types declared in the traversed schema.  Notice this function does not
-- currently handle namespaces in a correct manner, possibly allowing
-- for bad environment lookups when used on complex schemas.
buildTypeEnvironment :: Applicative m => (TypeName -> m Type) -> Type -> TypeName -> m Type
buildTypeEnvironment failure from =
    \forTy -> case HashMap.lookup forTy mp of
                Nothing  -> failure forTy
                Just res -> pure res
  where
  mp = HashMap.fromList $ go from
  go :: Type -> [(TypeName,Type)]
  go ty =
    let mk :: TypeName -> [TypeName] -> Maybe Text -> [(TypeName,Type)]
        mk n as ns =
            let unqual = n:as
                qual   = maybe [] (\x -> P.map (mappend (TN x <> ".")) unqual) ns
            in zip (unqual ++ qual) (repeat ty)
    in case ty of
        Record {..} -> mk name aliases namespace ++ concatMap (go . fldType) fields
        Enum {..}   -> mk name aliases namespace
        Union {..}  -> concatMap go options
        Fixed {..}  -> mk name aliases namespace
        Array {..}  -> go item
        _           -> []

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
