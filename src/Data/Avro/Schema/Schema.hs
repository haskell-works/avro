{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
-- {-# LANGUAGE StrictData            #-}

-- | Avro 'Schema's, represented here as values of type 'Schema',
-- describe the serialization and de-serialization of values.
--
-- In Avro schemas are compose-able such that encoding data under a schema and
-- decoding with a variant, such as newer or older version of the original
-- schema, can be accomplished by using the 'Data.Avro.Deconflict' module.
module Data.Avro.Schema.Schema
  (
   -- * Schema description types
    Schema(.., Int', Long', Bytes', String')
  , DefaultValue(..)
  , Field(..), Order(..)
  , TypeName(..)
  , Decimal(..)
  , LogicalTypeBytes(..), LogicalTypeFixed(..)
  , LogicalTypeInt(..), LogicalTypeLong(..)
  , LogicalTypeString(..)
  , renderFullname
  , parseFullname
  , mkEnum, mkUnion
  , validateSchema
  -- * Lower level utilities
  , typeName
  , buildTypeEnvironment
  , extractBindings

  , Result(..)
  , badValue
  , resultToEither

  , matches

  , parseBytes
  , serializeBytes

  , parseAvroJSON

  , overlay
  , subdefinition
  , expandNamedTypes
  ) where

import           Control.Applicative
import           Control.DeepSeq            (NFData)
import           Control.Monad.Except
import qualified Control.Monad.Fail         as MF
import           Control.Monad.State.Strict

import           Data.Aeson             (FromJSON (..), ToJSON (..), object, (.!=), (.:), (.:!), (.:?), (.=))
import qualified Data.Aeson             as A
import           Data.Aeson.Types       (Parser, typeMismatch)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Char              as Char
import           Data.Function          (on)
import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Int
import qualified Data.IntMap            as IM
import qualified Data.List              as L
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (catMaybes, fromMaybe, isJust)
import           Data.Monoid            (First (..))
import           Data.Semigroup
import qualified Data.Set               as S
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     as T
import qualified Data.Vector            as V
import           Prelude                as P

import GHC.Generics (Generic)

data DefaultValue
      = DNull
      | DBoolean !Bool
      | DInt Schema {-# UNPACK #-} Int32
      | DLong Schema {-# UNPACK #-} Int64
      | DFloat Schema {-# UNPACK #-} Float
      | DDouble Schema {-# UNPACK #-} Double
      | DBytes Schema {-# UNPACK #-} B.ByteString
      | DString Schema {-# UNPACK #-} Text
      | DArray (V.Vector DefaultValue)                   -- ^ Dynamically enforced monomorphic type.
      | DMap (HashMap Text DefaultValue)               -- ^ Dynamically enforced monomorphic type
      | DRecord Schema (HashMap Text DefaultValue) -- Order and a map
      | DUnion (V.Vector Schema) Schema DefaultValue -- ^ Set of union options, schema for selected option, and the actual value.
      | DFixed Schema {-# UNPACK #-} !B.ByteString
      | DEnum Schema {-# UNPACK #-} Int Text  -- ^ An enum is a set of the possible symbols (the schema) and the selected symbol
  deriving (Eq, Ord, Show, Generic, NFData)

-- | N.B. It is possible to create a Haskell value (of 'Schema' type) that is
-- not a valid Avro schema by violating one of the above or one of the
-- conditions called out in 'validateSchema'.
data Schema
      =
      -- Basic types
        Null
      | Boolean
      | Int    { logicalTypeI :: Maybe LogicalTypeInt }
      | Long   { logicalTypeL :: Maybe LogicalTypeLong }
      | Float | Double
      | Bytes  { logicalTypeB :: Maybe LogicalTypeBytes }
      | String { logicalTypeS :: Maybe LogicalTypeString }
      | Array  { item :: Schema }
      | Map    { values :: Schema }
      | NamedType TypeName
      -- Declared types
      | Record { name    :: TypeName
               , aliases :: [TypeName]
               , doc     :: Maybe Text
               , fields  :: [Field]
               }
      | Enum { name    :: TypeName
             , aliases :: [TypeName]
             , doc     :: Maybe Text
             , symbols :: V.Vector Text
             }
      | Union { options     :: V.Vector Schema
              }
      | Fixed { name         :: TypeName
              , aliases      :: [TypeName]
              , size         :: Int
              , logicalTypeF :: Maybe LogicalTypeFixed
              }
    deriving (Ord, Show, Generic, NFData)

pattern Int'    = Int    Nothing
pattern Long'   = Long   Nothing
pattern Bytes'  = Bytes  Nothing
pattern String' = String Nothing

data Field = Field { fldName    :: Text
                   , fldAliases :: [Text]
                   , fldDoc     :: Maybe Text
                   , fldOrder   :: Maybe Order
                   , fldType    :: Schema
                   , fldDefault :: Maybe DefaultValue
                   }
  deriving (Eq, Ord, Show, Generic, NFData)

data Order = Ascending | Descending | Ignore
  deriving (Eq, Ord, Show, Generic, NFData)

data Decimal
  = Decimal { precision :: Integer, scale :: Integer }
  deriving (Eq, Show, Ord, Generic, NFData)

newtype LogicalTypeBytes
  = DecimalB Decimal
  deriving (Eq, Show, Ord, Generic, NFData)

data LogicalTypeFixed
  = DecimalF Decimal | Duration
  deriving (Eq, Show, Ord, Generic, NFData)

data LogicalTypeInt
  = DecimalI Decimal | Date | TimeMillis
  deriving (Eq, Show, Ord, Generic, NFData)

data LogicalTypeLong
  = DecimalL Decimal | TimeMicros | TimestampMillis | TimestampMicros
  deriving (Eq, Show, Ord, Generic, NFData)

data LogicalTypeString
  = UUID
  deriving (Eq, Show, Ord, Generic, NFData)

instance Eq Schema where
  Null == Null = True
  Boolean == Boolean = True
  Int lt1 == Int lt2 = lt1 == lt2
  Long lt1 == Long lt2 = lt1 == lt2
  Float == Float = True
  Double == Double = True
  Bytes lt1 == Bytes lt2 = lt1 == lt2
  String lt1 == String lt2 = lt1 == lt2

  Array ty == Array ty2 = ty == ty2
  Map ty == Map ty2 = ty == ty2
  NamedType t == NamedType t2 = t == t2

  Record name1 _ _ fs1 == Record name2 _ _ fs2 =
    (name1 == name2) && (fs1 == fs2)
  Enum name1 _ _ s == Enum name2 _ _ s2 =
    (name1 == name2) && (s == s2)
  Union a == Union b = a == b
  Fixed name1 _ s lt1 == Fixed name2 _ s2 lt2 =
    (name1 == name2) && (s == s2) && (lt1 == lt2)

  _ == _ = False

-- | Build an 'Enum' value from its components.
mkEnum :: TypeName
          -- ^ The name of the enum (includes namespace).
       -> [TypeName]
          -- ^ Aliases for the enum (if any).
       -> Maybe Text
          -- ^ Optional documentation for the enum.
       -> [Text]
          -- ^ The symbols of the enum.
       -> Schema
mkEnum name aliases doc symbols = Enum name aliases doc (V.fromList symbols)

-- | @mkUnion subTypes@ Defines a union of the provided subTypes.  N.B. it is
-- invalid Avro to include another union or to have more than one of the same
-- type as a direct member of the union.  No check is done for this condition!
mkUnion :: NonEmpty Schema -> Schema
mkUnion  = Union . V.fromList . NE.toList

-- | A named type in Avro has a name and, optionally, a namespace.
--
-- A name is a string that starts with an ASCII letter or underscore
-- followed by letters, underscores and digits:
--
-- @
-- name ::= [A-Za-z_][A-Za-z0-9_]*
-- @
--
-- Examples include @"_foo7"@, @"Bar_"@ and @"x"@.
--
-- A namespace is a sequence of names with the same lexical
-- structure. When written as a string, the components of a namespace
-- are separated with dots (@"com.example"@).
--
-- 'TypeName' represents a /fullname/—a name combined with a
-- namespace. These are written and parsed as dot-separated
-- strings. The 'TypeName' @TN "Foo" ["com", "example"]@ is rendered
-- as @"com.example.Foo"@.
--
-- Fullnames have to be globally unique inside an Avro schema.
--
-- A namespace of @[]@ or @[""]@ is the "null namespace". In avro
-- an explicitly null-namespaced identifier is written as ".Foo"
data TypeName = TN { baseName  :: T.Text
                   , namespace :: [T.Text]
                   }
  deriving (Eq, Ord, Generic, NFData)

-- | Show the 'TypeName' as a string literal compatible with its
-- 'IsString' instance.
instance Show TypeName where
  show = show . renderFullname

-- | Render a fullname as a dot separated string.
--
-- @
-- > renderFullname (TN "Foo" ["com", "example"])
-- "com.example.Foo"
-- @
--
-- @
-- > renderFullname (TN "Foo" [])
-- ".Foo"
-- @
renderFullname :: TypeName -> T.Text
renderFullname TN { baseName, namespace } =
  T.intercalate "." $ namespace ++ [baseName]

-- | Parses a fullname into a 'TypeName', assuming the string
-- representation is valid.
--
-- @
-- > parseFullname "com.example.Foo"
-- TN { baseName = "Foo", components = ["com", "example"] }
-- @
parseFullname :: T.Text -> TypeName
parseFullname (T.splitOn "." -> components) = TN { baseName, namespace }
  where
    baseName  = last components
    namespace = filter (/= "") (init components)

-- | Build a type name out of the @name@ and @namespace@ fields of an
-- Avro record, enum or fixed definition.
--
-- This follows the rules laid out in the Avro specification:
--
--  1. If the @"name"@ field contains dots, it is parsed as a
--  /fullname/ (see 'parseFullname') and the @"namespace"@ field is
--  ignored if present.
--
--  2. Otherwise, if both @"name"@ and @"namespace"@ fields are
--  present, they make up the /fullname/
--
--  3. If only the @"name"@ field is specified, the @"namespace"@ is
--  inferred from the namespace of the most tightly enclosing schema
--  or protocol (the "context"). If there is no containing schema, the
--  namespace is null.
mkTypeName :: Maybe TypeName
              -- ^ The name of the enclosing schema or protocol, if
              -- any. This provides the context for inferring
              -- namespaces.
           -> Text
              -- ^ The @"name"@ field of the definition.
           -> Maybe Text
              -- ^ The @"namespace"@ field of the definition, if
              -- present.
           -> TypeName
              -- ^ The resulting /fullname/ of the generated type,
              -- according to the rules laid out above.
mkTypeName context name ns
  | isFullName name = parseFullname name
  | otherwise       = case ns of
      Just ns -> TN name $ filter (/= "") (T.splitOn "." ns)
      Nothing -> TN name $ maybe [] namespace context
  where isFullName = isJust . T.find (== '.')

-- | This lets us write 'TypeName's as string literals in a fully
-- qualified style. @"com.example.foo"@ is the name @"foo"@ with the
-- namespace @"com.example"@; @"foo"@ is the name @"foo"@ with no
-- namespace.
instance IsString TypeName where
  fromString = parseFullname . fromString

instance Hashable TypeName where
  hashWithSalt s (renderFullname -> name) =
    hashWithSalt (hashWithSalt s ("AvroTypeName" :: Text)) name

-- |Get the name of the type.  In the case of unions, get the name of the
-- first value in the union schema.
typeName :: Schema -> Text
typeName bt =
  case bt of
    Null            -> "null"
    Boolean         -> "boolean"
    Int Nothing     -> "int"
    Int (Just (DecimalI d))
                    -> decimalName d
    Int (Just Date) -> "date"
    Int (Just TimeMillis)
                    -> "time-millis"
    Long Nothing    -> "long"
    Long (Just (DecimalL d))
                    -> decimalName d
    Long (Just TimeMicros)
                    -> "time-micros"
    Long (Just TimestampMillis)
                    -> "timestamp-millis"
    Long (Just TimestampMicros)
                    -> "timestamp-micros"
    Float           -> "float"
    Double          -> "double"
    Bytes Nothing   -> "bytes"
    Bytes (Just (DecimalB d))
                    -> decimalName d
    String Nothing  -> "string"
    String (Just UUID)
                    -> "uuid"
    Array _         -> "array"
    Map   _         -> "map"
    NamedType name  -> renderFullname name
    Union ts        -> typeName (V.head ts)
    Fixed _ _ _ (Just (DecimalF d))
                    -> decimalName d
    Fixed _ _ _ (Just Duration)
                    -> "duration"
    _               -> renderFullname $ name bt
  where
    decimalName (Decimal prec sc) = "decimal(" <> T.pack (show prec) <> "," <> T.pack (show sc) <> ")"

instance FromJSON Schema where
  parseJSON = parseSchemaJSON Nothing

-- | A helper function that parses an Avro schema from JSON, resolving
-- namespaces based on context.
--
-- See 'mkTypeName' for details on how namespaces are resolved.
parseSchemaJSON :: Maybe TypeName
                -- ^ The name of the enclosing type of this schema, if
                -- any. Used to resolve namespaces.
                -> A.Value
                -- ^ An Avro schema encoded in JSON.
                -> Parser Schema
parseSchemaJSON context = \case
  A.String s -> case s of
    "null"             -> return Null
    "boolean"          -> return Boolean
    "int"              -> return $ Int Nothing
    "long"             -> return $ Long Nothing
    "float"            -> return Float
    "double"           -> return Double
    "bytes"            -> return $ Bytes Nothing
    "string"           -> return $ String Nothing
    "uuid"             -> return $ String (Just UUID)
    "date"             -> return $ Int (Just Date)
    "time-millis"      -> return $ Int (Just TimeMillis)
    "time-micros"      -> return $ Long (Just TimeMicros)
    "timestamp-millis" -> return $ Long (Just TimestampMillis)
    "timestamp-micros" -> return $ Long (Just TimestampMicros)
    somename           -> return $ NamedType $ mkTypeName context somename Nothing
  A.Array arr
    | V.length arr > 0 ->
      Union <$> V.mapM (parseSchemaJSON context) arr
    | otherwise        -> fail "Unions must have at least one type."
  A.Object o -> do
    logicalType :: Maybe Text <- o .:? "logicalType"
    ty                        <- o .: "type"

    case logicalType of
      Just "decimal" -> do
        prec <- o .: "precision"
        sc   <- fromMaybe 0 <$> o .:? "scale"
        let dec = Decimal prec sc
        case ty of
          "bytes" -> pure $ Bytes (Just (DecimalB dec))
          "fixed" -> (\fx -> fx { logicalTypeF = Just (DecimalF dec) }) <$> parseFixed o
          "int"   -> pure $ Int (Just (DecimalI dec))
          "long"  -> pure $ Long (Just (DecimalL dec))
          s       -> fail $ "Unsupported underlying type: " <> T.unpack s
      Just "uuid" -> case ty of
          "string" -> pure $ String (Just UUID)
          s        -> fail $ "Unsupported underlying type: " <> T.unpack s
      Just "date" -> case ty of
          "int" -> pure $ Int (Just Date)
          s     -> fail $ "Unsupported underlying type: " <> T.unpack s
      Just "time-millis" -> case ty of
          "int" -> pure $ Int (Just TimeMillis)
          s     -> fail $ "Unsupported underlying type: " <> T.unpack s
      Just "time-micros" -> case ty of
          "long" -> pure $ Long (Just TimeMicros)
          s      -> fail $ "Unsupported underlying type: " <> T.unpack s
      Just "timestamp-millis" -> case ty of
          "long" -> pure $ Long (Just TimestampMillis)
          s      -> fail $ "Unsupported underlying type: " <> T.unpack s
      Just "timestamp-micros" -> case ty of
          "long" -> pure $ Long (Just TimestampMicros)
          s      -> fail $ "Unsupported underlying type: " <> T.unpack s
      Just "duration" -> case ty of
          "fixed" -> (\fx -> fx { logicalTypeF = Just Duration }) <$> parseFixed o
          s       -> fail $ "Unsupported underlying type: " <> T.unpack s
      Just _  -> parseJSON (A.String ty)
      Nothing -> case ty of
        "map"    -> Map <$> (parseSchemaJSON context =<< o .: "values")
        "array"  -> Array <$> (parseSchemaJSON context =<< o .: "items")
        "record" -> do
          name      <- o .: "name"
          namespace <- o .:? "namespace"
          let typeName = mkTypeName context name namespace
              mkAlias name = mkTypeName (Just typeName) name Nothing
          aliases <- mkAliases typeName <$> (o .:? "aliases" .!= [])
          doc     <- o .:? "doc"
          fields  <- mapM (parseField typeName) =<< (o .: "fields")
          pure $ Record typeName aliases doc fields
        "enum"   -> do
          name      <- o .: "name"
          namespace <- o .:? "namespace"
          let typeName = mkTypeName context name namespace
              mkAlias name = mkTypeName (Just typeName) name Nothing
          aliases <- mkAliases typeName <$> (o .:? "aliases" .!= [])
          doc     <- o .:? "doc"
          symbols <- o .: "symbols"
          pure $ mkEnum typeName aliases doc symbols
        "fixed"   -> parseFixed o
        "null"    -> pure Null
        "boolean" -> pure Boolean
        "int"     -> pure $ Int Nothing
        "long"    -> pure $ Long Nothing
        "float"   -> pure Float
        "double"  -> pure Double
        "bytes"   -> pure $ Bytes Nothing
        "string"  -> pure $ String Nothing
        s        -> fail $ "Unrecognized object type: " <> T.unpack s

  invalid    -> typeMismatch "Invalid JSON for Avro Schema" invalid

  where
    parseFixed o = do
      name      <- o .: "name"
      namespace <- o .:? "namespace"
      let typeName = mkTypeName context name namespace
          mkAlias name = mkTypeName (Just typeName) name Nothing
      aliases <- mkAliases typeName <$> (o .:? "aliases" .!= [])
      size    <- o .: "size"
      pure $ Fixed typeName aliases size Nothing

-- | Parse aliases, inferring the namespace based on the type being aliases.
mkAliases :: TypeName
             -- ^ The name of the type being aliased.
          -> [Text]
             -- ^ The aliases.
          -> [TypeName]
mkAliases context = map $ \ name ->
  mkTypeName (Just context) name Nothing

-- | A helper function that parses field definitions, using the name
-- of the record for namespace resolution (see 'mkTypeName' for more
-- details).
parseField :: TypeName
              -- ^ The name of the record this field belongs to.
           -> A.Value
              -- ^ The JSON object defining the field in the schema.
           -> Parser Field
parseField record = \case
  A.Object o -> do
    name  <- o .: "name"
    doc   <- o .:? "doc"
    ty    <- parseSchemaJSON (Just record) =<< o .: "type"
    let err = error "Haskell Avro bindings does not support default for aliased or recursive types at this time."
    defM  <- o .:! "default"
    def   <- case parseFieldDefault err ty <$> defM of
      Just (Success x) -> return (Just x)
      Just (Error e)   -> fail e
      Nothing          -> return Nothing
    order <- o .:? ("order" :: Text)    .!= Just Ascending

    let mkAlias name = mkTypeName (Just record) name Nothing
    aliases  <- o .:? "aliases"  .!= []
    return $ Field name aliases doc order ty def
  invalid    -> typeMismatch "Field" invalid

instance ToJSON Schema where
  toJSON = schemaToJSON Nothing

-- | Serializes a 'Schema' to JSON.
--
-- The optional name is used as the context for namespace
-- inference. If the context has the namespace @com.example@, then any
-- names in the @com.example@ namespace will be rendered without an
-- explicit namespace.
schemaToJSON :: Maybe TypeName
                -- ^ The context used for keeping track of namespace
                -- inference.
             -> Schema
                -- ^ The schema to serialize to JSON.
             -> A.Value
schemaToJSON context = \case
  Null            -> A.String "null"
  Boolean         -> A.String "boolean"
  Int Nothing     -> A.String "int"
  Int (Just (DecimalI (Decimal prec sc))) ->
    object [ "type" .= ("int" :: Text), "logicalType" .= ("decimal" :: Text)
           , "precision" .= prec, "scale" .= sc ]
  Int (Just Date) ->
    object [ "type" .= ("int" :: Text), "logicalType" .= ("date" :: Text) ]
  Int (Just TimeMillis) ->
    object [ "type" .= ("int" :: Text), "logicalType" .= ("time-millis" :: Text) ]
  Long Nothing    -> A.String "long"
  Long (Just (DecimalL (Decimal prec sc))) ->
    object [ "type" .= ("long" :: Text), "logicalType" .= ("decimal" :: Text)
           , "precision" .= prec, "scale" .= sc ]
  Long (Just TimeMicros) ->
    object [ "type" .= ("long" :: Text), "logicalType" .= ("time-micros" :: Text) ]
  Long (Just TimestampMillis) ->
    object [ "type" .= ("long" :: Text), "logicalType" .= ("timestamp-millis" :: Text) ]
  Long (Just TimestampMicros) ->
    object [ "type" .= ("long" :: Text), "logicalType" .= ("timestamp-micros" :: Text) ]
  Float           -> A.String "float"
  Double          -> A.String "double"
  Bytes Nothing   -> A.String "bytes"
  Bytes (Just (DecimalB (Decimal prec sc))) ->
    object [ "type" .= ("bytes" :: Text), "logicalType" .= ("decimal" :: Text)
           , "precision" .= prec, "scale" .= sc ]
  String Nothing  -> A.String "string"
  String (Just UUID) ->
    object [ "type" .= ("string" :: Text), "logicalType" .= ("uuid" :: Text) ]
  Array tn        ->
    object [ "type" .= ("array" :: Text), "items" .= schemaToJSON context tn ]
  Map tn          ->
    object [ "type" .= ("map" :: Text), "values" .= schemaToJSON context tn ]
  NamedType name  -> toJSON $ render context name
  Record {..}     ->
    let opts = catMaybes
          [ ("doc" .=)   <$> doc
          ]
    in object $ opts ++
       [ "type"    .= ("record" :: Text)
       , "name"    .= render context name
       , "aliases" .= (render (Just name) <$> aliases)
       , "fields"  .= (fieldToJSON name <$> fields)
       ]
  Enum   {..} ->
    let opts = catMaybes [("doc" .=) <$> doc]
    in object $ opts ++
       [ "type"    .= ("enum" :: Text)
       , "name"    .= render context name
       , "aliases" .= (render (Just name) <$> aliases)
       , "symbols" .= symbols
       ]
  Union  {..} -> toJSON $ schemaToJSON context <$> options
  Fixed  {..} ->
    let basic =
           [ "type"    .= ("fixed" :: Text)
           , "name"    .= render context name
           , "aliases" .= (render (Just name) <$> aliases)
           , "size"    .= size
           ]
        extended = case logicalTypeF of
          Nothing       -> []
          Just Duration -> [ "logicalType" .= ("duration" :: Text) ]
          Just (DecimalF (Decimal prec sc))
                   -> [ "logicalType" .= ("decimal" :: Text)
                      , "precision" .= prec, "scale" .= sc ]
    in object (basic ++ extended)
  where render context typeName
          | Just ctx <- context
          , namespace ctx == namespace typeName = baseName typeName
          | otherwise                           = renderFullname typeName

        fieldToJSON context Field {..} =
          let opts = catMaybes
                [ ("order" .=)     <$> fldOrder
                , ("doc" .=)       <$> fldDoc
                , ("default" .=)   <$> fmap adjustDefaultValue fldDefault
                ]
          in object $ opts ++
             [ "name"    .= fldName
             , "type"    .= schemaToJSON (Just context) fldType
             , "aliases" .= fldAliases
             ]

        -- Default values for unions are encoded differently:
        -- the default value always represents the first element of a union
        adjustDefaultValue (DUnion _ _ val) = val
        adjustDefaultValue ty               = ty

instance ToJSON DefaultValue where
  toJSON av =
    case av of
      DNull            -> A.Null
      DBoolean b       -> A.Bool b
      DInt _ i         -> A.Number (fromIntegral i)
      DLong _ i        -> A.Number (fromIntegral i)
      DFloat _ f       -> A.Number (realToFrac f)
      DDouble _ d      -> A.Number (realToFrac d)
      DBytes _ bs      -> A.String (serializeBytes bs)
      DString _ t      -> A.String t
      DArray vec       -> A.Array (V.map toJSON vec)
      DMap mp          -> A.Object (HashMap.map toJSON mp)
      DRecord _ flds   -> A.Object (HashMap.map toJSON flds)
      DUnion _ _ DNull -> A.Null
      DUnion _ ty val  -> object [ typeName ty .= val ]
      DFixed _ bs      -> A.String (serializeBytes bs)
      DEnum _ _ txt    -> A.String txt

data Result a = Success a | Error String
  deriving (Eq, Ord, Show, Generic, NFData)

badValue :: Show t => t -> String -> Result a
badValue v t = fail $ "Unexpected value for '" <> t <> "': " <> show v

resultToEither :: Result b -> Either String b
resultToEither r =
  case r of
    Success v -> Right v
    Error err -> Left err
{-# INLINE resultToEither #-}

instance Monad Result where
  return = pure
  Success a >>= k = k a
  Error e >>= _ = Error e
#if !MIN_VERSION_base(4,13,0)
  fail = MF.fail
#endif
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
  mplus _ b             = b
instance Semigroup (Result a) where
  (<>) = mplus
instance Monoid (Result a) where
  mempty = fail "Empty Result"
  mappend = (<>)
instance Foldable Result where
  foldMap _ (Error _)   = mempty
  foldMap f (Success y) = f y
  foldr _ z (Error _)   = z
  foldr f z (Success y) = f y z
instance Traversable Result where
  traverse _ (Error err) = pure (Error err)
  traverse f (Success v) = Success <$> f v

-- | Field defaults are in the normal Avro JSON format except for
-- unions. Default values for unions are specified as JSON encodings
-- of the first type in the union.
parseFieldDefault :: (TypeName -> Maybe Schema)
                     -- ^ Lookup function for names defined in schema.
                  -> Schema
                     -- ^ The schema of the default value being parsed.
                  -> A.Value
                     -- ^ JSON encoding of an Avro value.
                  -> Result DefaultValue
parseFieldDefault env schema value = parseAvroJSON defaultUnion env schema value
  where defaultUnion (Union ts) val = DUnion ts (V.head ts) <$> parseFieldDefault env (V.head ts) val
        defaultUnion _ _            = error "Impossible: not Union."

-- | Parse JSON-encoded avro data.
parseAvroJSON :: (Schema -> A.Value -> Result DefaultValue)
                 -- ^ How to handle unions. The way unions are
                 -- formatted in JSON depends on whether we're parsing
                 -- a normal Avro object or we're parsing a default
                 -- declaration in a schema.
                 --
                 -- This function will only ever be passed 'Union'
                 -- schemas. It /should/ error out if this is not the
                 -- case—it represents a bug in this code.
              -> (TypeName -> Maybe Schema)
              -> Schema
              -> A.Value
              -> Result DefaultValue
parseAvroJSON union env (NamedType name) av =
  case env name of
    Nothing -> fail $ "Could not resolve type name for " <> T.unpack (renderFullname name)
    Just t  -> parseAvroJSON union env t av
parseAvroJSON union _ u@Union{} av             = u `union` av
parseAvroJSON union env ty av                  =
    case av of
      A.String s      ->
        case ty of
          String _    -> return $ DString ty s
          Enum {..}   ->
              case s `V.elemIndex` symbols of
                Just i  -> pure $ DEnum ty i s
                Nothing -> fail $ "JSON string is not one of the expected symbols for enum '" <> show name <> "': " <> T.unpack s
          Bytes _     -> DBytes ty <$> parseBytes s
          Fixed {..}  -> do
            bytes <- parseBytes s
            let len = B.length bytes
            when (len /= size) $
              fail $ "Fixed string wrong size. Expected " <> show size <> " but got " <> show len
            return $ DFixed ty bytes
          _ -> fail $ "Expected type String, Enum, Bytes, or Fixed, but found (Type,Value)="
             <> show (ty, av)
      A.Bool b       -> case ty of
                          Boolean -> return $ DBoolean b
                          _       -> avroTypeMismatch ty "boolean"
      A.Number i     ->
        case ty of
          Int _  -> return $ DInt    ty (floor i)
          Long _ -> return $ DLong   ty (floor i)
          Float  -> return $ DFloat  ty (realToFrac i)
          Double -> return $ DDouble ty (realToFrac i)
          _      -> avroTypeMismatch ty "number"
      A.Array vec    ->
        case ty of
          Array t -> DArray <$> V.mapM (parseAvroJSON union env t) vec
          _       -> avroTypeMismatch ty "array"
      A.Object obj ->
        case ty of
          Map mTy     -> DMap <$> mapM (parseAvroJSON union env mTy) obj
          Record {..} ->
           do let lkAndParse f =
                    case HashMap.lookup (fldName f) obj of
                      Nothing -> case fldDefault f of
                                  Just v  -> return v
                                  Nothing -> fail $ "Decode failure: No record field '" <> T.unpack (fldName f) <> "' and no default in schema."
                      Just v  -> parseAvroJSON union env (fldType f) v
              DRecord ty . HashMap.fromList <$> mapM (\f -> (fldName f,) <$> lkAndParse f) fields
          _ -> avroTypeMismatch ty "object"
      A.Null -> case ty of
                  Null -> return DNull
                  _    -> avroTypeMismatch ty "null"

-- | Parses a string literal into a bytestring in the format expected
-- for bytes and fixed values. Will fail if every character does not
-- have a codepoint between 0 and 255.
parseBytes :: Text -> Result B.ByteString
parseBytes bytes = case T.find (not . inRange) bytes of
  Just badChar -> fail $ "Invalid character in bytes or fixed string representation: " <> show badChar
  Nothing      -> return $ B.pack $ fromIntegral . Char.ord <$> T.unpack bytes
  where inRange (Char.ord -> c) = c >= 0x00 && c <= 0xFF

-- | Turn a 'ByteString' into a 'Text' that matches the format Avro
-- expects from bytes and fixed literals in JSON. Each byte is mapped
-- to a single Unicode codepoint between 0 and 255.
serializeBytes :: B.ByteString -> Text
serializeBytes = T.pack . map (Char.chr . fromIntegral) . B.unpack

avroTypeMismatch :: Schema -> Text -> Result a
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
-- the types declared in the traversed schema.
--
-- This mapping includes both the base type names and any aliases they
-- have. Aliases and normal names are not differentiated in any way.
buildTypeEnvironment :: Applicative m
                     => (TypeName -> m Schema)
                        -- ^ Callback to handle type names not in the
                        -- schema.
                     -> Schema
                        -- ^ The schema that we're generating a lookup
                        -- function for.
                     -> (TypeName -> m Schema)
buildTypeEnvironment failure from =
    \ forTy -> case HashMap.lookup forTy env of
                 Nothing  -> failure forTy
                 Just res -> pure res
  where
    env = extractBindings from

-- | Checks that two schemas match. This is like equality of schemas,
-- except 'NamedTypes' match against other types /with the same name/.
--
-- This extends recursively: two records match if they have the same
-- name, the same number of fields and the fields all match.
matches :: Schema -> Schema -> Bool
matches n@NamedType{} t             = typeName n == typeName t
matches t n@NamedType{}             = typeName t == typeName n
matches (Array itemA) (Array itemB) = matches itemA itemB
matches a@Record{} b@Record{}       =
  and [ name a == name b
      , length (fields a) == length (fields b)
      , and $ zipWith fieldMatches (fields a) (fields b)
      ]
  where fieldMatches = matches `on` fldType
matches a@Union{} b@Union{}         = and $ V.zipWith matches (options a) (options b)
matches t1 t2                       = t1 == t2

-- | @extractBindings schema@ traverses a schema and builds a map of all declared
-- types.
--
-- Types declared implicitly in record field definitions are also included. No distinction
-- is made between aliases and normal names.
extractBindings :: Schema -> HashMap.HashMap TypeName Schema
extractBindings = \case
  t@Record{..} ->
    let withRecord = HashMap.fromList $ (name : aliases) `zip` repeat t
    in HashMap.unions $ withRecord : (extractBindings . fldType <$> fields)
  e@Enum{..}   -> HashMap.fromList $ (name : aliases) `zip` repeat e
  Union{..}    -> HashMap.unions $ V.toList $ extractBindings <$> options
  f@Fixed{..}  -> HashMap.fromList $ (name : aliases) `zip` repeat f
  Array{..}    -> extractBindings item
  Map{..}      -> extractBindings values
  _            -> HashMap.empty


expandNamedTypes :: Schema -> Schema
expandNamedTypes =
  flip evalState HashMap.empty . go
  where
    expandField f@Field{fldType} = (\x -> f { fldType = x }) <$> go fldType
    go = \case
      t@(NamedType n)   -> fromMaybe t <$> gets (HashMap.lookup n)
      a@Array{item}     -> (\x -> a { item = x })   <$> go item
      m@Map{values}     -> (\x -> m { values = x }) <$> go values
      u@Union{options}  -> Union <$> traverse go options

      r@Record{name, fields}  -> do
        fields' <- traverse expandField fields
        let r' = r { fields = fields' }
        modify' (HashMap.insert name r')
        pure r'

      r@Enum{name} -> do
        modify' (HashMap.insert name r)
        pure r

      other -> pure other

-- | Merge two schemas to produce a third.
-- Specifically, @overlay schema reference@ fills in 'NamedTypes' in 'schema' using any matching definitions from 'reference'.
overlay :: Schema -> Schema -> Schema
overlay input supplement = overlayType input
  where
    overlayField f@Field{..}      = f { fldType = overlayType fldType }
    overlayType  a@Array{..}      = a { item    = overlayType item }
    overlayType  m@Map{..}        = m { values  = overlayType values }
    overlayType  r@Record{..}     = r { fields  = map overlayField fields }
    overlayType  u@Union{..}      = Union (fmap overlayType options)
    overlayType  nt@(NamedType _) = rebind nt
    overlayType  other            = other

    rebind (NamedType tn) = HashMap.lookupDefault (NamedType tn) tn bindings
    bindings              = extractBindings supplement

-- | Extract the named inner type definition as its own schema.
subdefinition :: Schema -> Text -> Maybe Schema
subdefinition schema name = mkTypeName Nothing name Nothing `HashMap.lookup` extractBindings schema
