{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module lets us derive Haskell types from an Avro schema that
-- can be serialized/deserialzed to Avro.
module Data.Avro.Deriving
  ( deriveAvro
  , deriveAvroWithNamespaces
  , deriveAvro'

  , deriveFromAvro
  , deriveFromAvroWithNamespaces
  , deriveFromAvro'
  )
where

import           Control.Monad                 (join)
import           Data.Aeson                    (eitherDecode)
import qualified Data.Aeson                    as J
import           Data.Avro                     hiding (decode, encode)
import           Data.Avro.Schema              as S
import qualified Data.Avro.Types               as AT
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import           Data.Char                     (isAlphaNum)
import           Data.Int
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as NE
import           Data.Map                      (Map)
import           Data.Maybe                    (fromMaybe)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as Text

import           Language.Haskell.TH           as TH
import           Language.Haskell.TH.Syntax

import           Data.Avro.Deriving.NormSchema

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LBSC8
import qualified Data.HashMap.Strict           as HM
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Vector                   as V

-- | How to treat Avro namespaces in the generated Haskell types.
data NamespaceBehavior =
    IgnoreNamespaces
    -- ^ Namespaces are ignored completely. Haskell identifiers are
    -- generated from types' base names. This produces nicer types but
    -- fails on valid Avro schemas where the same base name occurs in
    -- different namespaces.
  | HandleNamespaces
    -- ^ Haskell types and field names are generated with
    -- namespaces. See 'deriveAvroWithNamespaces' for an example of
    -- how this works.

-- | Derives Haskell types from the given Avro schema file. These
-- Haskell types support both reading and writing to Avro.
--
-- For an Avro schema with a top-level record called
-- @com.example.Foo@, this generates:
--
--   * a 'Schema' with the name @schema'Foo@
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
deriveAvro :: FilePath -> Q [Dec]
deriveAvro p = readSchema p >>= deriveAvro' IgnoreNamespaces

-- | Derives Haskell types from the given Avro schema file. These
-- Haskell types support both reading and writing to Avro.
--
-- For an Avro schema with a top-level record called
-- @com.example.Foo@, this generates:
--
--   * a 'Schema' with the name @schema'com'example'Foo@
--
--   * Haskell types for each named type defined in the schema
--     * 'HasSchema' instances for each type
--     * 'FromAvro' instances for each type
--     * 'ToAvro' instances for each type
--
-- This function uses /fullnames/ (name + namespace) to generate
-- Haskell type and field names. This results in more verbose Haskell
-- identifiers but will work correctly for most Avro schemas. (The one
-- exception is schemas that have types with the same fullnames with
-- different capitalization in the first letter.)
--
-- A type like @com.example.Foo@ turns into the Haskell type
-- @Com'example'Foo@.
--
-- Field names are transformed similarly, corrected for
-- capitalization. If 'com.example.Foo' has a field called 'Bar', the
-- generated record in Haskell will have a field called
-- @com'example'FooBar@.
deriveAvroWithNamespaces :: FilePath -> Q [Dec]
deriveAvroWithNamespaces p = readSchema p >>= deriveAvro' HandleNamespaces

-- | Derive Haskell types from the given Avro schema with configurable
-- behavior for handling namespaces.
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
--
-- See 'deriveAvro' and 'deriveAvroWithNamespaces' for details on
-- namespace handling.
deriveAvro' :: NamespaceBehavior -> Schema -> Q [Dec]
deriveAvro' namespaceBehavior s = do
  let schemas = extractDerivables s
  types     <- traverse (genType namespaceBehavior) schemas
  hasSchema <- traverse (genHasAvroSchema namespaceBehavior) schemas
  fromAvros <- traverse (genFromAvro namespaceBehavior) schemas
  toAvros   <- traverse (genToAvro namespaceBehavior) schemas
  pure $ join types <> join hasSchema <> join fromAvros <> join toAvros

-- | Derives "read only" Avro from a given schema file. For a schema
-- with a top-level definition @com.example.Foo@, this generates:
--
--  * a 'Schema' value with the name @schema'Foo@
--
--  * Haskell types for each named type defined in the schema
--    * 'HasSchema' instances for each type
--    * 'FromAvro' instances for each type
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
deriveFromAvro :: FilePath -> Q [Dec]
deriveFromAvro p = readSchema p >>= deriveFromAvro' IgnoreNamespaces

-- | Derives "read only" Avro from a given schema file. For a schema
-- with a top-level definition @com.example.Foo@, this generates:
--
--  * a 'Schema' value with the name @schema'com'example'Foo@
--
--  * Haskell types for each named type defined in the schema
--    * 'HasSchema' instances for each type
--    * 'FromAvro' instances for each type
--
-- This function uses /fullnames/ (name + namespace) to generate
-- Haskell type and field names. This results in more verbose Haskell
-- identifiers but will work correctly for most Avro schemas. (The one
-- exception is schemas that have types with the same fullnames with
-- different capitalization in the first letter.)
--
-- A type like @com.example.Foo@ turns into the Haskell type
-- @Com'example'Foo@.
--
-- Field names are transformed similarly, corrected for
-- capitalization. If 'com.example.Foo' has a field called 'Bar', the
-- generated record in Haskell will have a field called
-- @com'example'FooBar@.
deriveFromAvroWithNamespaces :: FilePath -> Q [Dec]
deriveFromAvroWithNamespaces p = readSchema p >>= deriveFromAvro' HandleNamespaces

-- | Derive "read only" Haskell types from the given Avro schema with
-- configurable behavior for handling namespaces.
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
--
-- See 'deriveAvro' and 'deriveAvroWithNamespaces' for details on
-- namespace handling.
deriveFromAvro' :: NamespaceBehavior -> Schema -> Q [Dec]
deriveFromAvro' namespaceBehavior s = do
  let schemas = extractDerivables s
  types     <- traverse (genType namespaceBehavior) schemas
  hasSchema <- traverse (genHasAvroSchema namespaceBehavior) schemas
  fromAvros <- traverse (genFromAvro namespaceBehavior) schemas
  pure $ join types <> join hasSchema <> join fromAvros

readSchema :: FilePath -> Q Schema
readSchema p = do
  qAddDependentFile p
  mbSchema <- runIO $ decodeSchema p
  case mbSchema of
    Left err  -> fail $ "Unable to generate AVRO for " <> p <> ": " <> err
    Right sch -> pure sch

genFromAvro :: NamespaceBehavior -> Schema -> Q [Dec]
genFromAvro namespaceBehavior (S.Enum n _ _ _ _) =
  [d| instance FromAvro $(conT $ mkDataTypeName namespaceBehavior n) where
        fromAvro (AT.Enum _ i _) = $([| pure . toEnum|]) i
        fromAvro value           = $( [|\v -> badValue v $(mkTextLit $ S.renderFullname n)|] ) value
  |]
genFromAvro namespaceBehavior (S.Record n _ _ _ fs) =
  [d| instance FromAvro $(conT $ mkDataTypeName namespaceBehavior n) where
        fromAvro (AT.Record _ r) =
           $(genFromAvroFieldsExp (mkDataTypeName namespaceBehavior n) fs) r
        fromAvro value           = $( [|\v -> badValue v $(mkTextLit $ S.renderFullname n)|] ) value
  |]
genFromAvro namespaceBehavior (S.Fixed n _ s) =
  [d| instance FromAvro $(conT $ mkDataTypeName namespaceBehavior n) where
        fromAvro (AT.Fixed _ v)
          | BS.length v == s = pure $ $(conE (mkDataTypeName namespaceBehavior n)) v
        fromAvro value = $( [|\v -> badValue v $(mkTextLit $ S.renderFullname n)|] ) value
  |]
genFromAvro _ _                             = pure []

genFromAvroFieldsExp :: Name -> [Field] -> Q Exp
genFromAvroFieldsExp n (x:xs) =
  [| \r ->
    $(let extract fld = [| r .: T.pack $(mkTextLit (fldName fld))|]
          ctor = [| $(conE n) <$> $(extract x) |]
      in foldl (\expr fld -> [| $expr <*> $(extract fld) |]) ctor xs
     )
  |]

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

genToAvro :: NamespaceBehavior -> Schema -> Q [Dec]
genToAvro namespaceBehavior s@(Enum n _ _ vs _) =
  toAvroInstance (mkSchemaValueName namespaceBehavior n)
  where
    conP' = flip conP [] . mkAdtCtorName namespaceBehavior n
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName namespaceBehavior n) where
            toAvro = $([| \x ->
              let convert = AT.Enum $(varE sname) (fromEnum $([|x|]))
              in $(caseE [|x|] ((\v -> match (conP' v)
                               (normalB [| convert (T.pack $(mkTextLit v))|]) []) <$> vs))
              |])
      |]

genToAvro namespaceBehavior s@(Record n _ _ _ fs) =
  toAvroInstance (mkSchemaValueName namespaceBehavior n)
  where
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName namespaceBehavior n) where
            toAvro = $(genToAvroFieldsExp sname)
      |]
    genToAvroFieldsExp sname = [| \r -> record $(varE sname)
        $(let assign fld = [| T.pack $(mkTextLit (fldName fld)) .= $(varE $ mkFieldTextName namespaceBehavior n fld) r |]
          in listE $ assign <$> fs
        )
      |]

genToAvro namespaceBehavior s@(Fixed n _ size) =
  toAvroInstance (mkSchemaValueName namespaceBehavior n)
  where
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName namespaceBehavior n) where
            toAvro = $(do
              x <- newName "x"
              lamE [conP (mkDataTypeName namespaceBehavior n) [varP x]] [| AT.Fixed $(varE sname) $(varE x) |])
      |]

schemaDef :: Name -> Schema -> Q [Dec]
schemaDef sname sch = setName sname $
  [d|
      x :: Schema
      x = $(mkSchema sch)
  |]
  where mkSchema = \case
          Null           -> [e| Null |]
          Boolean        -> [e| Boolean |]
          Int            -> [e| Int |]
          Long           -> [e| Long |]
          Float          -> [e| Float |]
          Double         -> [e| Double |]
          Bytes          -> [e| Bytes |]
          String         -> [e| String |]
          Array item     -> [e| Array $(mkSchema item) |]
          Map values     -> [e| Map $(mkSchema values) |]
          NamedType name -> [e| NamedType $(mkName name) |]
          Record {..}    -> [e| Record { name      = $(mkName name)
                                       , aliases   = $(ListE <$> mapM mkName aliases)
                                       , doc       = $(mkMaybeText doc)
                                       , order     = $(mkOrder order)
                                       , fields    = $(ListE <$> mapM mkField fields)
                                       }
                              |]
          Enum {..}      -> [e| mkEnum $(mkName name)
                                       $(ListE <$> mapM mkName aliases)
                                       $(mkMaybeText doc)
                                       $(ListE <$> mapM mkText symbols)
                              |]
          Union {..}     -> [e| mkUnion $(mkNE options) |]
          Fixed {..}     -> [e| Fixed { name      = $(mkName name)
                                      , aliases   = $(ListE <$> mapM mkName aliases)
                                      , size      = $(litE $ IntegerL $ fromIntegral size)
                                      }
                              |]

        mkText text = [e| T.pack $(mkTextLit text) |]

        mkName (TN name namespace) = [e| TN $(mkText name) $(mkNamespace namespace) |]
        mkNamespace ls = listE $ stringE . T.unpack <$> ls

        mkMaybeText (Just text) = [e| Just $(mkText text) |]
        mkMaybeText Nothing     = [e| Nothing |]

        mkOrder (Just Ascending)  = [e| Just Ascending |]
        mkOrder (Just Descending) = [e| Just Descending |]
        mkOrder (Just Ignore)     = [e| Just Ignore |]
        mkOrder Nothing           = [e| Nothing |]

        mkField Field {..} =
          [e| Field { fldName    = $(mkText fldName)
                    , fldAliases = $(ListE <$> mapM mkText fldAliases)
                    , fldDoc     = $(mkMaybeText fldDoc)
                    , fldOrder   = $(mkOrder fldOrder)
                    , fldType    = $(mkSchema fldType)
                    , fldDefault = $(fromMaybe [e|Nothing|] $ mkJust . mkDefaultValue <$> fldDefault)
                    }
            |]

        mkJust exp = [e|Just $(exp)|]

        mkDefaultValue = \case
          AT.Null         -> [e| AT.Null |]
          AT.Boolean b    -> [e| AT.Boolean $(if b then [e|True|] else [e|False|]) |]
          AT.Int n        -> [e| AT.Int $(litE $ IntegerL $ fromIntegral n) |]
          AT.Long n       -> [e| AT.Long $(litE $ IntegerL $ fromIntegral n) |]
          AT.Float f      -> [e| AT.Long $(litE $ FloatPrimL $ realToFrac f) |]
          AT.Double f     -> [e| AT.Long $(litE $ FloatPrimL $ realToFrac f) |]
          AT.Bytes bs     -> [e| AT.Bytes $(mkByteString bs) |]
          AT.String s     -> [e| AT.String $(mkText s) |]
          AT.Array vec    -> [e| AT.Array $ V.fromList $(ListE <$> mapM mkDefaultValue (V.toList vec)) |]
          AT.Map m        -> [e| AT.Map $ $(mkMap m) |]
          AT.Record s m   -> [e| AT.Record $(mkSchema s) $(mkMap m) |]
          AT.Union _ _ v  -> mkDefaultValue v
          AT.Fixed s bs   -> [e| AT.Fixed $(mkSchema s) $(mkByteString bs) |]
          AT.Enum s n sym -> [e| AT.Enum $(mkSchema s) $(litE $ IntegerL $ fromIntegral n) $(mkText sym) |]

        mkByteString bs = [e| B.pack $(ListE <$> mapM numericLit (B.unpack bs)) |]
          where numericLit = litE . IntegerL . fromIntegral

        mkMap (HM.toList -> xs) = [e| HM.fromList $(ListE <$> mapM mkKVPair xs) |]
        mkKVPair (k, v)         = [e| ($(mkText k), $(mkDefaultValue v)) e|]

        mkNE (NE.toList -> xs) = [e| NE.fromList $(ListE <$> mapM mkSchema xs) |]

-- | A hack around TemplateHaskell limitation:
-- It is currently not possible to splice variable name in QQ.
-- This function allows to replace hardcoded name into the specified one.
setName :: Name -> Q [Dec] -> Q [Dec]
setName = fmap . map . sn
  where
    sn n (SigD _ t)          = SigD n t
    sn n (ValD (VarP _) x y) = ValD (VarP n) x y
    sn _ d                   = d

genType :: NamespaceBehavior -> Schema -> Q [Dec]
genType namespaceBehavior (S.Record n _ _ _ fs) = do
  flds <- traverse (mkField namespaceBehavior n) fs
  let dname = mkDataTypeName namespaceBehavior n
  sequenceA [genDataType dname flds]
genType namespaceBehavior (S.Enum n _ _ vs _) = do
  let dname = mkDataTypeName namespaceBehavior n
  sequenceA [genEnum dname (mkAdtCtorName namespaceBehavior n <$> vs)]
genType namespaceBehavior (S.Fixed n _ s) = do
  let dname = mkDataTypeName namespaceBehavior n
  sequenceA [genNewtype dname]
genType _ _ = pure []

mkFieldTypeName :: NamespaceBehavior -> S.Type -> Q TH.Type
mkFieldTypeName namespaceBehavior t = case t of
  S.Boolean                     -> [t| Bool |]
  S.Long                        -> [t| Int64 |]
  S.Int                         -> [t| Int32 |]
  S.Float                       -> [t| Float |]
  S.Double                      -> [t| Double |]
  S.Bytes                       -> [t| ByteString |]
  S.String                      -> [t| Text |]
  S.Union (Null :| [x]) _       -> [t| Maybe $(mkFieldTypeName namespaceBehavior x) |]
  S.Union (x :| [Null]) _       -> [t| Maybe $(mkFieldTypeName namespaceBehavior x) |]
  S.Union (x :| [y]) _          -> [t| Either $(mkFieldTypeName namespaceBehavior x) $(mkFieldTypeName namespaceBehavior y) |]
  S.Union (_ :| _) _            -> error "Unions with more than 2 elements are not yet supported"
  S.Record n _ _ _ _            -> [t| $(conT $ mkDataTypeName namespaceBehavior n) |]
  S.Map x                       -> [t| Map Text $(mkFieldTypeName namespaceBehavior x) |]
  S.Array x                     -> [t| [$(mkFieldTypeName namespaceBehavior x)] |]
  S.NamedType n                 -> [t| $(conT $ mkDataTypeName namespaceBehavior n)|]
  S.Fixed n _ _                 -> [t| $(conT $ mkDataTypeName namespaceBehavior n)|]
  S.Enum n _ _ _ _              -> [t| $(conT $ mkDataTypeName namespaceBehavior n)|]
  _                             -> error $ "Avro type is not supported: " <> show t

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

mkFieldTextName :: NamespaceBehavior -> TypeName -> Field -> Name
mkFieldTextName namespaceBehavior (renderName namespaceBehavior -> dn) fld =
  mkTextName . sanitiseName $ updateFirst T.toLower dn
                           <> updateFirst T.toUpper (fldName fld)

mkField :: NamespaceBehavior -> TypeName -> Field -> Q VarStrictType
mkField namespaceBehavior prefix field = do
  ftype <- mkFieldTypeName namespaceBehavior (fldType field)
  let fName = mkFieldTextName namespaceBehavior prefix field
  pure (fName, defaultStrictness, ftype)

genNewtype :: Name -> Q Dec
#if MIN_VERSION_template_haskell(2,12,0)
genNewtype dn = do
  ders <- sequenceA [[t|Eq|], [t|Show|]]
  fldType <- [t|ByteString|]
  let ctor = RecC dn [(mkName ("un" ++ nameBase dn), defaultStrictness, fldType)]
  pure $ NewtypeD [] dn [] Nothing ctor [DerivClause Nothing ders]
#elif MIN_VERSION_template_haskell(2,11,0)
genNewtype dn = do
  ders <- sequenceA [[t|Eq|], [t|Show|]]
  fldType <- [t|ByteString|]
  let ctor = RecC dn [(mkName ("un" ++ nameBase dn), defaultStrictness, fldType)]
  pure $ NewtypeD [] dn [] Nothing ctor ders
#else
genNewtype dn = do
  [ConT eq, ConT sh] <- sequenceA [[t|Eq|], [t|Show|]]
  fldType <- [t|ByteString|]
  let ctor = RecC dn [(mkName ("un" ++ nameBase dn), defaultStrictness, fldType)]
  pure $ NewtypeD [] dn [] ctor [eq, sh]
#endif

genEnum :: Name -> [Name] -> Q Dec
#if MIN_VERSION_template_haskell(2,12,0)
genEnum dn vs = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|]]
  pure $ DataD [] dn [] Nothing ((\n -> NormalC n []) <$> vs) [DerivClause Nothing ders]
#elif MIN_VERSION_template_haskell(2,11,0)
genEnum dn vs = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|]]
  pure $ DataD [] dn [] Nothing ((\n -> NormalC n []) <$> vs) ders
#else
genEnum dn vs = do
  [ConT eq, ConT sh, ConT or, ConT en] <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|]]
  pure $ DataD [] dn [] ((\n -> NormalC n []) <$> vs) [eq, sh, or, en]
#endif

genDataType :: Name -> [VarStrictType] -> Q Dec
#if MIN_VERSION_template_haskell(2,12,0)
genDataType dn flds = do
  ders <- sequenceA [[t|Eq|], [t|Show|]]
  pure $ DataD [] dn [] Nothing [RecC dn flds] [DerivClause Nothing ders]
#elif MIN_VERSION_template_haskell(2,11,0)
genDataType dn flds = do
  ders <- sequenceA [[t|Eq|], [t|Show|]]
  pure $ DataD [] dn [] Nothing [RecC dn flds] ders
#else
genDataType dn flds = do
  [ConT eq, ConT sh] <- sequenceA [[t|Eq|], [t|Show|]]
  pure $ DataD [] dn [] [RecC dn flds] [eq, sh]
#endif

defaultStrictness :: Strict
#if MIN_VERSION_template_haskell(2,11,0)
defaultStrictness = Bang SourceNoUnpack NoSourceStrictness
#else
defaultStrictness = NotStrict
#endif

mkTextName :: Text -> Name
mkTextName = mkName . T.unpack

mkLit :: String -> ExpQ
mkLit = litE . StringL

mkTextLit :: Text -> ExpQ
mkTextLit = litE . StringL . T.unpack
