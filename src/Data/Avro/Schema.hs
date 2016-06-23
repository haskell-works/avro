{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Avro Schemas, represented here as Haskell values of type Schema,
-- provide a method to serialize values, deserialize bytestrings to values
-- and are composable such that an encoding schema and decoding schema can
-- be combined to yield a new decoder.
module Data.Avro.Schema
  (
   -- * Schema description types
    Schema(..), Type(..), BasicType(..)
  , DeclaredType(..), Field(..), Order(..)
  , TypeName(..)
  , validateSchema
  -- * Values and smart constructors for common 'Type's
  , long, int, Data.Avro.Schema.null, boolean, float, double, bytes, string, namedType
  , array, Data.Avro.Schema.map, union
  -- * Lower level utilities
  , buildTypeEnvironment
  , Result(..)
  ) where

import           Prelude as P
import           Control.Applicative
import           Control.Monad.Except
import qualified Control.Monad.Fail as MF
import qualified Data.Aeson as A
import           Data.Aeson ((.=),object,(.:?),(.:),(.!=),FromJSON(..),ToJSON(..))
import           Data.Aeson.Types (Parser,typeMismatch)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable
import           Data.Monoid ((<>))
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Avro.Types as Ty

-- |An Avro schema is either
-- * A "JSON object in the form `{"type":"typeName" ...` (DeclaredType that is not Union)
-- * A "JSON string, naming a defined type" (Basic Type w/o free variables/names)
-- * A "JSON array, representing a union" (DeclaredType w/ Union constr)
--
-- N.B. It is possible to create a Haskell value (of Schema type) that is
-- not a valid Avro schema by violating one of the above or one of the
-- conditions called out in 'validateSchema'.
data Schema = Schema Type
  deriving (Eq, Show)

-- |Avro types are either primitive (string, int, etc)or declared
-- (structures, unions etc).
data Type = BasicType BasicType | DeclaredType DeclaredType
  deriving (Eq, Show)

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
  hashWithSalt s (TN t) = hashWithSalt s t

-- Get the name of the type.  In the case of unions, get the name of the
-- first value in the union schema.
typeName :: Type -> Text
typeName (BasicType bt) =
  case bt of
    Null     -> "null"
    Boolean  -> "boolean"
    Int      -> "int"
    Long     -> "long"
    Float    -> "float"
    Double   -> "double"
    Bytes    -> "bytes"
    String   -> "string"
    Array _  -> "array"
    Map   _  -> "map"
    NamedType (TN t) -> t
typeName (DeclaredType dt) =
  case dt of
     Union (x:_) -> typeName x
     Union []    -> error "Invalid Schema used: Union of zero types." -- XXX
     _           -> unTN $ name dt

-- | An enumeration of the primitive types provided by Avro
data BasicType
      = Null
      | Boolean
      | Int   | Long
      | Float | Double
      | Bytes | String
      | Array { item :: Type }
      | Map   { values :: Type }
      | NamedType TypeName
  deriving (Eq, Show)

long, int, null, boolean, float, double, bytes, string :: Type
namedType :: TypeName -> Type
array,map :: Type -> Type
long    = BasicType Long
int     = BasicType Int
null    = BasicType Null
boolean = BasicType Boolean
float   = BasicType Float
double  = BasicType Double
bytes   = BasicType Bytes
string  = BasicType String
namedType = BasicType . NamedType
array   = BasicType . Array
map     = BasicType . Map

union :: [Type] -> Type
union ts = DeclaredType (Union ts)

-- | The more complex types are records (product types), enum, union (sum
-- types), and Fixed (fixed length vector of bytes).
data DeclaredType
      = Record { name      :: TypeName
               , namespace :: Maybe Text
               , doc       :: Maybe Text
               , aliases   :: [TypeName]
               , order     :: Maybe Order
               , fields    :: [Field]
               }
      | Enum { name      :: TypeName
             , aliases   :: [TypeName]
             , namespace :: Maybe Text
             , doc       :: Maybe Text
             , symbols   :: [Text]
             }
      | Union { options  :: [Type]
              }
      | Fixed { name        :: TypeName
              , namespace   :: Maybe Text
              , aliases     :: [TypeName]
              , size        :: Integer
              }
    deriving (Eq, Show)

data Field = Field { fldName       :: Text
                   , fldDoc        :: Maybe Text
                   , fldType       :: Type
                   , fldDefault    :: Maybe (Ty.Value Type)
                   , fldOrder      :: Maybe Order
                   , fldAliases    :: [Text]
                   }
  deriving (Eq, Show)

data Order = Ascending | Descending | Ignore
  deriving (Eq, Ord, Show)

instance FromJSON Schema where
  parseJSON val =
    case val of
      o@(A.Object _) -> Schema <$> parseJSON o
      A.Array arr    -> Schema . DeclaredType . Union . V.toList <$> mapM parseJSON arr
      _              -> typeMismatch "JSON Schema" val

instance ToJSON Schema where
  toJSON (Schema x) = toJSON x

instance FromJSON Type where
  parseJSON (A.String s) =
    BasicType <$> case s of
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
        "map"    -> (BasicType . Map)   <$> o .: ("values" :: Text)
        "array"  -> (BasicType . Array) <$> o .: ("items"  :: Text)
        "record" ->
          DeclaredType <$>
          (Record <$> o .:  ("name" :: Text)
                 <*> o .:? ("namespace" :: Text)
                 <*> o .:? ("doc" :: Text)
                 <*> o .:? ("aliases" :: Text)  .!= []
                 <*> o .:? ("order" :: Text) .!= Just Ascending
                 <*> o .:  ("fields" :: Text))
        "enum"   ->
          DeclaredType <$>
          (Enum <$> o .:  ("name" :: Text)
               <*> o .:? ("aliases" :: Text)  .!= []
               <*> o .:? ("namespace" :: Text)
               <*> o .:? ("doc" :: Text)
               <*> o .:  ("symbols" :: Text))
        "fixed"  ->
           DeclaredType <$> (Fixed
                 <$> o .:  ("name" :: Text)
                 <*> o .:? ("namespace" :: Text)
                 <*> o .:? ("aliases" :: Text) .!= []
                 <*> o .:  ("size" :: Text))
        s  -> fail $ "Unrecognized object type: " <> s
  parseJSON (A.Array arr) =
           DeclaredType . Union <$> mapM parseJSON (V.toList arr)
  parseJSON foo = typeMismatch "Invalid JSON for Avro Schema" foo

instance ToJSON Type where
  toJSON (BasicType bt)    = toJSON bt
  toJSON (DeclaredType dt) = toJSON dt

instance ToJSON BasicType where
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

instance ToJSON DeclaredType where
  toJSON dt =
    case dt of
      Record {..} ->
        object [ "type"      .= ("record" :: Text)
               , "name"      .= name
               , "aliases"   .= aliases
               , "fields"    .= fields
               , "order"     .= order
               , "namespace" .= namespace
               , "doc"       .= doc
               ]
      Enum   {..} ->
        object [ "type"      .= ("enum" :: Text)
               , "name"      .= name
               , "aliases"   .= aliases
               , "doc"       .= doc
               , "namespace" .= namespace
               , "symbols"   .= symbols
               ]
      Union  {..} -> A.Array $ V.fromList $ P.map toJSON options
      Fixed  {..} ->
        object [ "type"      .= ("fixed" :: Text)
               , "name"      .= name
               , "namespace" .= namespace
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
       return $ Field nm doc ty def od al

  parseJSON j = typeMismatch "Field " j

instance ToJSON Field where
  toJSON (Field {..}) =
    object [ "name"    .= fldName
           , "doc"     .= fldDoc
           , "type"    .= fldType
           , "default" .= fldDefault
           , "order"   .= fldOrder
           , "aliases" .= fldAliases
           ]

instance ToJSON (Ty.Value Type) where
  toJSON av =
    case av of
      Ty.Null            -> A.Null
      Ty.Boolean b       -> A.Bool b
      Ty.Int i           -> A.Number (fromIntegral i)
      Ty.Long i          -> A.Number (fromIntegral i)
      Ty.Float f         -> A.Number (realToFrac f)
      Ty.Double d        -> A.Number (realToFrac d)
      Ty.Bytes bs        -> A.String ("\\u" <> T.decodeUtf8 (Base16.encode bs))
      Ty.String t        -> A.String t
      Ty.Array vec       -> A.Array (V.map toJSON vec)
      Ty.Map mp          -> A.Object (HashMap.map toJSON mp)
      Ty.Record flds     -> A.Object (HashMap.map toJSON flds)
      Ty.Union _ _ Ty.Null -> A.Null
      Ty.Union _ ty val    -> object [ typeName ty .= val ]
      Ty.Fixed bs        -> A.String ("\\u" <> T.decodeUtf8 (Base16.encode bs))  -- XXX the example wasn't literal - this should be an actual bytestring... somehow.
      Ty.Enum _ txt      -> A.String txt

-- XXX remove and use 'Result' from Aeson?
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
parseAvroJSON env (BasicType (NamedType (TN tn))) av =
  case env tn of
    Nothing -> fail $ "Could not resolve type name for " <> show tn
    Just t  -> parseAvroJSON env t av
parseAvroJSON env ty av =
    case av of
      A.String s     ->
        case ty of
          BasicType String -> return $ Ty.String s
          DeclaredType (Enum {..}) ->
              if s `elem` symbols
                then return $ Ty.Enum ty s
                else fail $ "JSON string is not one of the expected symbols for enum '" <> show name <> "': " <> T.unpack s
          -- XXX unions with String/NamedType/Enum?
          _ -> avroTypeMismatch ty "string"
      A.Bool b       -> case ty of
                          BasicType Boolean -> return $ Ty.Boolean b
                          _       -> avroTypeMismatch ty "boolean"
      A.Number i     ->
        case ty of
          BasicType Int    -> return $ Ty.Int    (floor i)
          BasicType Long   -> return $ Ty.Long   (floor i)
          BasicType Float  -> return $ Ty.Float  (realToFrac i)
          BasicType Double -> return $ Ty.Double (realToFrac i)
          -- XXX unions with numbers?
          _                   -> avroTypeMismatch ty "number"
      A.Array vec    ->
        case ty of
          BasicType (Array t) -> Ty.Array <$> V.mapM (parseAvroJSON env t) vec
          -- XXX unions with arrays?
          _  -> avroTypeMismatch ty "array"
      A.Object obj ->
        case ty of
          BasicType (Map mTy)        -> Ty.Map <$> mapM (parseAvroJSON env mTy) obj
          DeclaredType (Record {..}) -> -- Ty.Record <$> HashMap.mapM (parseAvroJSON env rTy) obj
           do let lkAndParse f =
                    case HashMap.lookup (fldName f) obj of
                      Nothing -> case fldDefault f of
                                  Just v  -> return v
                                  Nothing -> fail $ "Decode failure: No record field '" <> T.unpack (fldName f) <> "' and no default in schema."
                      Just v  -> parseAvroJSON env (fldType f) v
              Ty.Record . HashMap.fromList <$> mapM (\f -> (fldName f,) <$> lkAndParse f) fields
              -- XXX unions with map or record?
          _ -> avroTypeMismatch ty "object"
      A.Null -> case ty of
                  BasicType Null -> return $ Ty.Null
                  DeclaredType (Union us) | (BasicType Null) `elem` us -> return $ Ty.Union us (BasicType Null) Ty.Null
                  _ -> avroTypeMismatch ty "null"

avroTypeMismatch :: Type -> Text -> Result a
avroTypeMismatch expected actual =
  fail $ "Could not resolve type '" <> T.unpack actual <> "' with expected type: " <> show expected
-- TODO make into a GADT
-- data DefaultField
--     = DefNull
--     | DefBool Bool
--     | DefInt Integer
--     | DefFloat Scientific
--     | DefBytes Text
--     | DefString Text
--     | DefRecord Object
--     | DefEnum Text
--     | DefArray A.Array
--     | DefMap Object
--     | DefFixed Text
-- 
-- instance ToJSON DefaultField where
--   toJSON d =
--     case d of
--       DefNull     -> A.Null
--       DefBool b   -> Bool b
--       DefInt i    -> Number (fromIntegral i)
--       DefFloat f  -> Number f
--       DefBytes t  -> A.String t
--       DefString t -> A.String t
--       DefRecord o -> Object o
--       DefEnum t   -> A.String t
--       DefArray a  -> A.Array a
--       DefMap o    -> Object o
--       DefFixed t  -> A.String t

instance ToJSON Order where
  toJSON o =
    case o of
      Ascending  -> A.String "ascending"
      Descending -> A.String "descending"
      Ignore     -> A.String "ignore"

instance FromJSON Order where
  parseJSON (A.String s) =
    case s of
      "ascending"  -> return $ Ascending
      "descending" -> return $ Descending
      "ignore"     -> return $ Ignore
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
validateSchema :: Schema -> Parser ()
validateSchema _sch = return () -- XXX TODO

buildTypeEnvironment :: Applicative m => (TypeName -> m Type) -> Type -> TypeName -> m Type
buildTypeEnvironment failure from =
    \forTy -> case HashMap.lookup forTy mp of
                Nothing  -> failure forTy
                Just res -> pure res
  where
  mp = HashMap.fromList $ go from
  go :: Type -> [(TypeName,Type)]
  go (BasicType b) = []
  go ty@(DeclaredType d) =
    let mk :: TypeName -> [TypeName] -> Maybe Text -> [(TypeName,Type)]
        mk n as ns =
            let unqual = n:as
                qual   = maybe [] (\x -> P.map (mappend (TN x <> ".")) unqual) ns
            in zip (unqual ++ qual) (repeat ty)
    in case d of
        Record {..} -> mk name aliases namespace ++ concatMap go (P.map fldType fields)
        Enum {..}   -> mk name aliases namespace
        Union {..}  -> concatMap go options
        Fixed {..}  -> mk name aliases namespace
