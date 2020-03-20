{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | This module lets us derive Haskell types from an Avro schema that
-- can be serialized/deserialzed to Avro.
module Data.Avro.Deriving
  ( -- * Deriving options
    DeriveOptions(..)
  , FieldStrictness(..)
  , FieldUnpackedness(..)
  , NamespaceBehavior(..)
  , defaultDeriveOptions
  , mkPrefixedFieldName
  , mkAsIsFieldName
  , mkLazyField
  , mkStrictPrimitiveField

  -- * Deriving Haskell types from Avro schema
  , makeSchema
  , makeSchemaFrom
  , deriveAvroWithOptions
  , deriveAvroWithOptions'
  , deriveAvroFromByteString
  , deriveAvro
  , deriveAvro'

  -- * Re-exporting a quasiquoter for raw string literals
  , r
)
where

import           Control.Monad                 (join)
import           Control.Monad.Identity        (Identity)
import           Data.Aeson                    (eitherDecode)
import qualified Data.Aeson                    as J
import           Data.Avro                     hiding (decode, encode)
import           Data.Avro.Encoding.EncodeAvro (EncodeAvro (..), putI)
import           Data.Avro.Schema.Schema       as S
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import           Data.Char                     (isAlphaNum)
import qualified Data.Foldable                 as Foldable
import           Data.Int
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as NE
import           Data.Map                      (Map)
import           Data.Maybe                    (fromMaybe)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as Text
import           Data.Time                     (Day, DiffTime, UTCTime)
import           Data.UUID                     (UUID)
import           Text.RawString.QQ             (r)

import qualified Data.Avro.Encoding.DecodeAvro as AV

import GHC.Generics (Generic)

import Language.Haskell.TH        as TH hiding (notStrict)
import Language.Haskell.TH.Lib    as TH hiding (notStrict)
import Language.Haskell.TH.Syntax

import Data.Avro.Deriving.NormSchema
import Data.Avro.EitherN

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.HashMap.Strict        as HM
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as V

import Data.Avro.Deriving.Lift    ()
import Language.Haskell.TH.Syntax (lift)

-- | How to treat Avro namespaces in the generated Haskell types.
data NamespaceBehavior =
    IgnoreNamespaces
    -- ^ Namespaces are ignored completely. Haskell identifiers are
    -- generated from types' base names. This produces nicer types but
    -- fails on valid Avro schemas where the same base name occurs in
    -- different namespaces.
    --
    -- The Avro type @com.example.Foo@ would generate the Haskell type
    -- @Foo@. If @Foo@ had a field called @bar@, the generated Haskell
    -- record would have a field called @fooBar@.
  | HandleNamespaces
    -- ^ Haskell types and field names are generated with
    -- namespaces. See 'deriveAvroWithNamespaces' for an example of
    -- how this works.
    --
    -- The Avro type @com.example.Foo@ would generate the Haskell type
    -- @Com'example'Foo@. If @Foo@ had a field called @bar@, the
    -- generated Haskell record would have the field
    -- @com'example'FooBar@.

-- | Describes the strictness of a field for a derived
-- data type. The field will be derived as if it were
-- written with a @!@.
data FieldStrictness = StrictField | LazyField
  deriving Generic

-- | Describes the representation of a field for a derived
-- data type. The field will be derived as if it were written
-- with an @{-# UNPACK #-}@ pragma.
data FieldUnpackedness = UnpackedField | NonUnpackedField
  deriving Generic

-- | Derives Avro from a given schema file.
-- Generates data types, FromAvro and ToAvro instances.
data DeriveOptions = DeriveOptions
  { -- | How to build field names for generated data types. The first
    -- argument is the type name to use as a prefix, rendered
    -- according to the 'namespaceBehavior' setting.
    fieldNameBuilder    :: Text -> Field -> T.Text

    -- | Determines field representation of generated data types
  , fieldRepresentation :: TypeName -> Field -> (FieldStrictness, FieldUnpackedness)

    -- | Controls how we handle namespaces when defining Haskell type
    -- and field names.
  , namespaceBehavior   :: NamespaceBehavior
  } deriving Generic

-- | Default deriving options
--
-- @
-- defaultDeriveOptions = 'DeriveOptions'
--   { fieldNameBuilder  = 'mkPrefixedFieldName'
--   , fieldStrictness   = 'mkLazyField'
--   , namespaceBehavior = 'IgnoreNamespaces'
--   }
-- @
defaultDeriveOptions = DeriveOptions
  { fieldNameBuilder    = mkPrefixedFieldName
  , fieldRepresentation = mkLazyField
  , namespaceBehavior   = IgnoreNamespaces
  }

-- | Generates a field name that is prefixed with the type name.
--
-- For example, if the schema defines type 'Person' that has a field 'firstName',
-- then the generated Haskell type will be like
--
-- @
-- Person { personFirstName :: Text }
-- @
mkPrefixedFieldName :: Text -> Field -> T.Text
mkPrefixedFieldName prefix fld =
  sanitiseName $ updateFirst T.toLower prefix <> updateFirst T.toUpper (fldName fld)

-- | Marks any field as non-strict in the generated data types.
mkLazyField :: TypeName -> Field -> (FieldStrictness, FieldUnpackedness)
mkLazyField _ _ =
  (LazyField, NonUnpackedField)


-- | Make a field strict and unpacked if it has a primitive representation.
-- Primitive types are types which GHC has either a static or an unlifted
-- representation: `()`, `Boolean`, `Int32`, `Int64`, `Float`, `Double`.
mkStrictPrimitiveField :: TypeName -> Field -> (FieldStrictness, FieldUnpackedness)
mkStrictPrimitiveField _ field =
  if shouldStricten
  then (StrictField, unpackedness)
  else (LazyField, NonUnpackedField)
  where
    unpackedness =
      case S.fldType field of
        S.Null    -> NonUnpackedField
        S.Boolean -> NonUnpackedField
        _         -> UnpackedField

    shouldStricten =
      case S.fldType field of
        S.Null    -> True
        S.Boolean -> True
        S.Int _   -> True
        S.Long _  -> True
        S.Float   -> True
        S.Double  -> True
        _         -> False

-- | Generates a field name that matches the field name in schema
-- (sanitised for Haskell, so first letter is lower cased)
--
-- For example, if the schema defines type 'Person' that has a field 'firstName',
-- then the generated Haskell type will be like
--
-- @
-- Person { firstName :: Text }
-- @
-- You may want to enable 'DuplicateRecordFields' if you want to use this method.
mkAsIsFieldName :: Text -> Field -> Text
mkAsIsFieldName _ = sanitiseName . updateFirst T.toLower . fldName

-- | Derives Haskell types from the given Avro schema file. These
-- Haskell types support both reading and writing to Avro.
--
-- For an Avro schema with a top-level record called
-- @com.example.Foo@, this generates:
--
--   * a 'Schema' with the name @schema'Foo@ or
--     @schema'com'example'Foo@, depending on the 'namespaceBehavior'
--     setting.
--
--   * Haskell types for each named type defined in the schema
--     * 'HasSchema' instances for each type
--     * 'FromAvro' instances for each type
--     * 'ToAvro' instances for each type
--
-- This function ignores namespaces when generated Haskell type and
-- field names. This will fail on valid Avro schemas which contain
-- types with the same base name in different namespaces. It will also
-- fail for schemas that contain types with base names that are the
-- same except for the capitalization of the first letter.
--
-- The type @com.example.Foo@ will generate a Haskell type @Foo@. If
-- @com.example.Foo@ has a field named @Bar@, the field in the Haskell
-- record will be called @fooBar@.
deriveAvroWithOptions :: DeriveOptions -> FilePath -> Q [Dec]
deriveAvroWithOptions o p = readSchema p >>= deriveAvroWithOptions' o

-- | Derive Haskell types from the given Avro schema.
--
-- For an Avro schema with a top-level definition @com.example.Foo@, this
-- generates:
--
--   * a 'Schema' with the name @schema'Foo@ or
--     @schema'com'example'Foo@ depending on namespace handling
--
--   * Haskell types for each named type defined in the schema
--     * 'HasSchema' instances for each type
--     * 'FromAvro' instances for each type
--     * 'ToAvro' instances for each type
deriveAvroWithOptions' :: DeriveOptions -> Schema -> Q [Dec]
deriveAvroWithOptions' o s = do
  let schemas = extractDerivables s
  types       <- traverse (genType o) schemas
  hasSchema   <- traverse (genHasAvroSchema $ namespaceBehavior o) schemas
  fromValues  <- traverse (genFromValue $ namespaceBehavior o) schemas
  encodeAvros <- traverse (genEncodeAvro o) schemas
  pure $ join types <> join hasSchema <> join fromValues <> join encodeAvros

-- | Same as 'deriveAvroWithOptions' but uses 'defaultDeriveOptions'
--
-- @
-- deriveAvro = 'deriveAvroWithOptions' 'defaultDeriveOptions'
-- @
deriveAvro :: FilePath -> Q [Dec]
deriveAvro = deriveAvroWithOptions defaultDeriveOptions

-- | Same as 'deriveAvroWithOptions'' but uses 'defaultDeriveOptions'
--
-- @
-- deriveAvro' = 'deriveAvroWithOptions'' 'defaultDeriveOptions'
-- @
deriveAvro' :: Schema -> Q [Dec]
deriveAvro' = deriveAvroWithOptions' defaultDeriveOptions

-- | Same as 'deriveAvro' but takes a ByteString rather than FilePath
deriveAvroFromByteString :: LBS.ByteString -> Q [Dec]
deriveAvroFromByteString bs = case eitherDecode bs of
    Right schema -> deriveAvroWithOptions' defaultDeriveOptions schema
    Left err     -> fail $ "Unable to generate AVRO for bytestring: " <> err

-- | Generates the value of type 'Schema' that it can later be used with
-- 'deriveAvro'' or 'deriveAvroWithOptions''.
--
-- @
-- mySchema :: Schema
-- mySchema = $(makeSchema "schemas/my-schema.avsc")
-- @
makeSchema :: FilePath -> Q Exp
makeSchema p = readSchema p >>= lift

makeSchemaFrom :: FilePath -> Text -> Q Exp
makeSchemaFrom p name = do
  s <- readSchema p

  case subdefinition s name of
    Nothing -> fail $ "No such entity '" <> T.unpack name <> "' defined in " <> p
    Just ss -> lift ss

readSchema :: FilePath -> Q Schema
readSchema p = do
  qAddDependentFile p
  mbSchema <- runIO $ decodeSchema p
  case mbSchema of
    Left err  -> fail $ "Unable to generate AVRO for " <> p <> ": " <> err
    Right sch -> pure sch

---------------------------- New FromAvro -----------------------------------------

badValueNew :: Show v => v -> String -> Either String a
badValueNew v t = Left $ "Unexpected value for '" <> t <> "': " <> show v

genFromValue :: NamespaceBehavior -> Schema -> Q [Dec]
genFromValue namespaceBehavior (S.Enum n _ _ _ ) =
  [d| instance AV.DecodeAvro $(conT $ mkDataTypeName namespaceBehavior n) where
        fromValue (AV.Enum _ i _) = $([| pure . toEnum|]) i
        fromValue value           = $( [|\v -> badValueNew v $(mkTextLit $ S.renderFullname n)|] ) value
  |]
genFromValue namespaceBehavior (S.Record n _ _ _ fs) =
  [d| instance AV.DecodeAvro $(conT $ mkDataTypeName namespaceBehavior n) where
        fromValue (AV.Record r) =
           $(genFromAvroNewFieldsExp (mkDataTypeName namespaceBehavior n) fs) r
        fromValue value           = $( [|\v -> badValueNew v $(mkTextLit $ S.renderFullname n)|] ) value
  |]
genFromValue namespaceBehavior (S.Fixed n _ s _) =
  [d| instance AV.DecodeAvro $(conT $ mkDataTypeName namespaceBehavior n) where
        fromValue (AV.Fixed _ v)
          | BS.length v == s = pure $ $(conE (mkDataTypeName namespaceBehavior n)) v
        fromValue value = $( [|\v -> badValueNew v $(mkTextLit $ S.renderFullname n)|] ) value
  |]
genFromValue _ _                             = pure []

genFromAvroNewFieldsExp :: Name -> [Field] -> Q Exp
genFromAvroNewFieldsExp n xs =
  [| \r ->
    $(let ctor = [| pure $(conE n) |]
      in foldl (\expr (i, _) -> [| $expr <*> AV.fromValue (r V.! i) |]) ctor (zip [(0 :: Int)..] xs)
    )
  |]

----------------------- HasAvroSchema ----------------------------------------

genHasAvroSchema :: NamespaceBehavior -> Schema -> Q [Dec]
genHasAvroSchema namespaceBehavior s = do
  let sname = mkSchemaValueName namespaceBehavior (name s)
  sdef <- schemaDef sname s
  idef <- hasAvroSchema sname
  pure (sdef <> idef)
  where
    hasAvroSchema sname =
      [d| instance HasAvroSchema $(conT $ mkDataTypeName namespaceBehavior (name s)) where
            schema = pure $(varE sname)
      |]

newNames :: String
            -- ^ base name
         -> Int
            -- ^ count
         -> Q [Name]
newNames base n = sequence [newName (base ++ show i) | i <- [1..n]]

------------------------- EncodeAvro ------------------------------------------------

genEncodeAvro :: DeriveOptions -> Schema -> Q [Dec]
genEncodeAvro opts s@(S.Enum n _ _ _) =
  encodeAvroInstance (mkSchemaValueName (namespaceBehavior opts) n)
  where
    encodeAvroInstance sname =
      [d| instance EncodeAvro $(conT $ mkDataTypeName (namespaceBehavior opts) n) where
            toEncoding = $([| \_ x -> putI (fromEnum x) |])
      |]

genEncodeAvro opts s@(S.Record n _ _ _ fs) =
  encodeAvroInstance (mkSchemaValueName (namespaceBehavior opts) n)
  where
    encodeAvroInstance sname =
      [d| instance EncodeAvro $(conT $ mkDataTypeName (namespaceBehavior opts) n) where
            toEncoding = $(encodeAvroFieldsExp sname)
      |]
    encodeAvroFieldsExp sname = do
      names <- newNames "p_" (length fs)
      wn <- varP <$> newName "_"
      let con = conP (mkDataTypeName (namespaceBehavior opts) n) (varP <$> names)
      lamE [wn, con]
            [| mconcat $( let build (fld, n) = [| toEncoding (fldType fld) $(varE n) |]
                          in listE $ build <$> (zip fs names)
                        )
            |]

genEncodeAvro opts s@(S.Fixed n _ _ _) =
  encodeAvroInstance (mkSchemaValueName (namespaceBehavior opts) n)
  where
    encodeAvroInstance sname =
      [d| instance EncodeAvro $(conT $ mkDataTypeName (namespaceBehavior opts) n) where
            toEncoding = $(do
              x <- newName "x"
              wc <- newName "_"
              lamE [varP wc, conP (mkDataTypeName (namespaceBehavior opts) n) [varP x]] [| toEncoding $(varE sname) $(varE x) |])
      |]
genEncodeAvro _ _ = pure []

schemaDef :: Name -> Schema -> Q [Dec]
schemaDef sname sch = setName sname $
  [d|
      x :: Schema
      x = sch
  |]

-- | A hack around TemplateHaskell limitation:
-- It is currently not possible to splice variable name in QQ.
-- This function allows to replace hardcoded name into the specified one.
setName :: Name -> Q [Dec] -> Q [Dec]
setName = fmap . map . sn
  where
    sn n (SigD _ t)          = SigD n t
    sn n (ValD (VarP _) x y) = ValD (VarP n) x y
    sn _ d                   = d

genType :: DeriveOptions -> Schema -> Q [Dec]
genType opts (S.Record n _ _ _ fs) = do
  flds <- traverse (mkField opts n) fs
  let dname = mkDataTypeName (namespaceBehavior opts) n
  sequenceA [genDataType dname flds]
genType opts (S.Enum n _ _ vs) = do
  let dname = mkDataTypeName (namespaceBehavior opts) n
  sequenceA [genEnum dname (mkAdtCtorName (namespaceBehavior opts) n <$> (V.toList vs))]
genType opts (S.Fixed n _ s _) = do
  let dname = mkDataTypeName (namespaceBehavior opts) n
  sequenceA [genNewtype dname]
genType _ _ = pure []

mkFieldTypeName :: NamespaceBehavior -> S.Schema -> Q TH.Type
mkFieldTypeName namespaceBehavior = \case
  S.Boolean          -> [t| Bool |]
  S.Long (Just (DecimalL (Decimal p s)))
                     -> [t| Decimal $(litT $ numTyLit p) $(litT $ numTyLit s) |]
  S.Long (Just TimeMicros)
                     -> [t| DiffTime |]
  S.Long (Just TimestampMicros)
                     -> [t| UTCTime |]
  S.Long (Just TimestampMillis)
                     -> [t| UTCTime |]
  S.Long _           -> [t| Int64 |]
  S.Int (Just Date)  -> [t| Day |]
  S.Int (Just TimeMillis)
                     -> [t| DiffTime |]
  S.Int _            -> [t| Int32 |]
  S.Float            -> [t| Float |]
  S.Double           -> [t| Double |]
  S.Bytes _          -> [t| ByteString |]
  S.String Nothing   -> [t| Text |]
  S.String (Just UUID) -> [t| UUID |]
  S.Union branches   -> union (Foldable.toList branches)
  S.Record n _ _ _ _ -> [t| $(conT $ mkDataTypeName namespaceBehavior n) |]
  S.Map x            -> [t| Map Text $(go x) |]
  S.Array x          -> [t| [$(go x)] |]
  S.NamedType n      -> [t| $(conT $ mkDataTypeName namespaceBehavior n)|]
  S.Fixed n _ _ _    -> [t| $(conT $ mkDataTypeName namespaceBehavior n)|]
  S.Enum n _ _ _     -> [t| $(conT $ mkDataTypeName namespaceBehavior n)|]
  t                  -> error $ "Avro type is not supported: " <> show t
  where go = mkFieldTypeName namespaceBehavior
        union = \case
          []              ->
            error "Empty union types are not supported"
          [x]             -> [t| Identity $(go x) |]
          [Null, x]       -> [t| Maybe $(go x) |]
          [x, Null]       -> [t| Maybe $(go x) |]
          [x, y]          -> [t| Either $(go x) $(go y) |]
          [a, b, c]       -> [t| Either3 $(go a) $(go b) $(go c) |]
          [a, b, c, d]    -> [t| Either4 $(go a) $(go b) $(go c) $(go d) |]
          [a, b, c, d, e] -> [t| Either5 $(go a) $(go b) $(go c) $(go d) $(go e) |]
          [a, b, c, d, e, f] -> [t| Either6 $(go a) $(go b) $(go c) $(go d) $(go e) $(go f) |]
          [a, b, c, d, e, f, g] -> [t| Either7 $(go a) $(go b) $(go c) $(go d) $(go e) $(go f) $(go g)|]
          [a, b, c, d, e, f, g, h] -> [t| Either8 $(go a) $(go b) $(go c) $(go d) $(go e) $(go f) $(go g) $(go h)|]
          [a, b, c, d, e, f, g, h, i] -> [t| Either9 $(go a) $(go b) $(go c) $(go d) $(go e) $(go f) $(go g) $(go h) $(go i)|]
          [a, b, c, d, e, f, g, h, i, j] -> [t| Either10 $(go a) $(go b) $(go c) $(go d) $(go e) $(go f) $(go g) $(go h) $(go i) $(go j)|]
          ls              ->
            error $ "Unions with more than 10 elements are not yet supported: Union has " <> (show . length) ls <> " elements"

updateFirst :: (Text -> Text) -> Text -> Text
updateFirst f t =
  let (l, ls) = T.splitAt 1 t
  in f l <> ls

decodeSchema :: FilePath -> IO (Either String Schema)
decodeSchema p = eitherDecode <$> LBS.readFile p

mkAdtCtorName :: NamespaceBehavior -> TypeName -> Text -> Name
mkAdtCtorName namespaceBehavior prefix nm =
  concatNames (mkDataTypeName namespaceBehavior prefix) (mkDataTypeName' nm)

concatNames :: Name -> Name -> Name
concatNames a b = mkName $ nameBase a <> nameBase b

sanitiseName :: Text -> Text
sanitiseName =
  let valid c = isAlphaNum c || c == '\'' || c == '_'
  in T.concat . T.split (not . valid)

-- | Renders a fully qualified Avro name to a valid Haskell
-- identifier. This does not change capitalization—make sure to
-- capitalize as needed depending on whether the name is a Haskell
-- type, constructor, variable or field.
--
-- With 'HandleNamespaces', namespace components (if any) are
-- separated with @'@. The Avro name @"com.example.foo"@ would be
-- rendered as @com'example'foo@.
--
-- With 'IgnoreNamespaces', only the base name of the type is
-- used. The Avro name @"com.example.foo"@ would be rendered as
-- @"foo"@.
renderName :: NamespaceBehavior
              -- ^ How to handle namespaces when generating the type
              -- name.
           -> TypeName
              -- ^ The name to transform into a valid Haskell
              -- identifier.
           -> Text
renderName namespaceBehavior (TN name namespace) = case namespaceBehavior of
  HandleNamespaces -> Text.intercalate "'" $ namespace <> [name]
  IgnoreNamespaces -> name

mkSchemaValueName :: NamespaceBehavior -> TypeName -> Name
mkSchemaValueName namespaceBehavior typeName =
  mkTextName $ "schema'" <> renderName namespaceBehavior typeName

mkDataTypeName :: NamespaceBehavior -> TypeName -> Name
mkDataTypeName namespaceBehavior = mkDataTypeName' . renderName namespaceBehavior

mkDataTypeName' :: Text -> Name
mkDataTypeName' =
  mkTextName . sanitiseName . updateFirst T.toUpper . T.takeWhileEnd (/='.')

mkField :: DeriveOptions -> TypeName -> Field -> Q VarStrictType
mkField opts typeName field = do
  ftype <- mkFieldTypeName (namespaceBehavior opts) (fldType field)
  let prefix = renderName (namespaceBehavior opts) typeName
      fName = mkTextName $ (fieldNameBuilder opts) prefix field
      (fieldStrictness, fieldUnpackedness) =
        fieldRepresentation opts typeName field
      strictness =
        case fieldStrictness of
          StrictField -> strict fieldUnpackedness
          LazyField   -> notStrict

  pure (fName, strictness, ftype)

genNewtype :: Name -> Q Dec
#if MIN_VERSION_template_haskell(2,12,0)
genNewtype dn = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
  fldType <- [t|ByteString|]
  let ctor = RecC dn [(mkName ("un" ++ nameBase dn), notStrict, fldType)]
  pure $ NewtypeD [] dn [] Nothing ctor [DerivClause Nothing ders]
#elif MIN_VERSION_template_haskell(2,11,0)
genNewtype dn = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
  fldType <- [t|ByteString|]
  let ctor = RecC dn [(mkName ("un" ++ nameBase dn), notStrict, fldType)]
  pure $ NewtypeD [] dn [] Nothing ctor ders
#else
genNewtype dn = do
  [ConT eq, ConT sh, ConT gen] <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
  fldType <- [t|ByteString|]
  let ctor = RecC dn [(mkName ("un" ++ nameBase dn), notStrict, fldType)]
  pure $ NewtypeD [] dn [] ctor [eq, sh, gen]
#endif

genEnum :: Name -> [Name] -> Q Dec
#if MIN_VERSION_template_haskell(2,12,0)
genEnum dn vs = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|], [t|Bounded|], [t|Generic|]]
  pure $ DataD [] dn [] Nothing ((\n -> NormalC n []) <$> vs) [DerivClause Nothing ders]
#elif MIN_VERSION_template_haskell(2,11,0)
genEnum dn vs = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|], [t|Bounded|], [t|Generic|]]
  pure $ DataD [] dn [] Nothing ((\n -> NormalC n []) <$> vs) ders
#else
genEnum dn vs = do
  [ConT eq, ConT sh, ConT or, ConT en, ConT gen] <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|], [t|Bounded|], [t|Generic|]]
  pure $ DataD [] dn [] ((\n -> NormalC n []) <$> vs) [eq, sh, or, en, gen]
#endif

genDataType :: Name -> [VarStrictType] -> Q Dec
#if MIN_VERSION_template_haskell(2,12,0)
genDataType dn flds = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
  pure $ DataD [] dn [] Nothing [RecC dn flds] [DerivClause Nothing ders]
#elif MIN_VERSION_template_haskell(2,11,0)
genDataType dn flds = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
  pure $ DataD [] dn [] Nothing [RecC dn flds] ders
#else
genDataType dn flds = do
  [ConT eq, ConT sh, ConT gen] <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
  pure $ DataD [] dn [] [RecC dn flds] [eq, sh, gen]
#endif

notStrict :: Strict
#if MIN_VERSION_template_haskell(2,11,0)
notStrict = Bang SourceNoUnpack NoSourceStrictness
#else
notStrict = NotStrict
#endif

strict :: FieldUnpackedness -> Strict
#if MIN_VERSION_template_haskell(2,11,0)
strict UnpackedField    = Bang SourceUnpack SourceStrict
strict NonUnpackedField = Bang SourceNoUnpack SourceStrict
#else
strict UnpackedField    = Unpacked
strict NonUnpackedField = IsStrict
#endif

mkTextName :: Text -> Name
mkTextName = mkName . T.unpack

mkLit :: String -> ExpQ
mkLit = litE . StringL

mkTextLit :: Text -> ExpQ
mkTextLit = litE . StringL . T.unpack
