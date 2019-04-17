{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

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
  , deriveFromAvroWithOptions
  , deriveAvroFromByteString
  , deriveAvro
  , deriveAvro'
  , deriveFromAvro
)
where

import           Control.Monad      (join)
import           Data.Aeson         (eitherDecode)
import qualified Data.Aeson         as J
import           Data.Avro          hiding (decode, encode)
import           Data.Avro.Schema   as S
import qualified Data.Avro.Types    as AT
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
import           Data.Char          (isAlphaNum)
import           Data.Int
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Map           (Map)
import           Data.Maybe         (fromMaybe)
import           Data.Semigroup     ((<>))
import qualified Data.Text          as Text


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

import           Data.Avro.Decode.Lazy.FromLazyAvro
import qualified Data.Avro.Decode.Lazy.LazyValue    as LV

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
        S.Int     -> True
        S.Long    -> True
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
  types     <- traverse (genType o) schemas
  hasSchema <- traverse (genHasAvroSchema $ namespaceBehavior o) schemas
  fromAvros <- traverse (genFromAvro $ namespaceBehavior o) schemas
  fromLazyAvros <- traverse (genFromLazyAvro $ namespaceBehavior o) schemas
  toAvros   <- traverse (genToAvro o) schemas
  pure $ join types <> join hasSchema <> join fromAvros <> join fromLazyAvros <> join toAvros

-- | Derives "read only" Avro from a given schema file. For a schema
-- with a top-level definition @com.example.Foo@, this generates:
--
--  * a 'Schema' value with the name @schema'Foo@
--
--  * Haskell types for each named type defined in the schema
--    * 'HasSchema' instances for each type
--    * 'FromAvro' instances for each type
deriveFromAvroWithOptions :: DeriveOptions -> FilePath -> Q [Dec]
deriveFromAvroWithOptions o p = readSchema p >>= deriveFromAvroWithOptions' o

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
deriveFromAvroWithOptions' :: DeriveOptions -> Schema -> Q [Dec]
deriveFromAvroWithOptions' o s = do
  let schemas = extractDerivables s
  types     <- traverse (genType o) schemas
  hasSchema <- traverse (genHasAvroSchema $ namespaceBehavior o) schemas
  fromAvros <- traverse (genFromAvro $ namespaceBehavior o) schemas
  fromLazyAvros <- traverse (genFromLazyAvro $ namespaceBehavior o) schemas
  pure $ join types <> join hasSchema <> join fromAvros <> join fromLazyAvros

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

-- | Same as 'deriveFromAvroWithOptions' but uses
-- 'defaultDeriveOptions'.
--
-- @
-- deriveFromAvro = deriveFromAvroWithOptions defaultDeriveOptions
-- @
deriveFromAvro :: FilePath -> Q [Dec]
deriveFromAvro = deriveFromAvroWithOptions defaultDeriveOptions

-- | Generates the value of type 'Schema' that it can later be used with
-- 'deriveAvro'' or 'deriveAvroWithOptions''.
--
-- @
-- mySchema :: Schema
-- mySchema = $(makeSchema "schemas/my-schema.avsc")
-- @
makeSchema :: FilePath -> Q Exp
makeSchema p = readSchema p >>= schemaDef'

makeSchemaFrom :: FilePath -> Text -> Q Exp
makeSchemaFrom p name = do
  s <- readSchema p

  case subdefinition s name of
    Nothing -> fail $ "No such entity '" <> T.unpack name <> "' defined in " <> p
    Just ss -> schemaDef' ss

readSchema :: FilePath -> Q Schema
readSchema p = do
  qAddDependentFile p
  mbSchema <- runIO $ decodeSchema p
  case mbSchema of
    Left err  -> fail $ "Unable to generate AVRO for " <> p <> ": " <> err
    Right sch -> pure sch

---------------------------- FromAvro -----------------------------------------

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
genFromAvroFieldsExp n []     = [| (return . return) $(conE n) |]
genFromAvroFieldsExp n (x:xs) =
  [| \r ->
    $(let extract fld = [| r .: T.pack $(mkTextLit (fldName fld))|]
          ctor = [| $(conE n) <$> $(extract x) |]
      in foldl (\expr fld -> [| $expr <*> $(extract fld) |]) ctor xs
     )
  |]

-------------------------------- FromLazyAvro ---------------------------------
genFromLazyAvro :: NamespaceBehavior -> Schema -> Q [Dec]
genFromLazyAvro namespaceBehavior (S.Enum n _ _ _ _) =
  [d| instance FromLazyAvro $(conT $ mkDataTypeName namespaceBehavior n) where
        fromLazyAvro (LV.Enum _ i _) = $([| pure . toEnum|]) i
        fromLazyAvro value           = $( [|\v -> badValue v $(mkTextLit $ S.renderFullname n)|] ) value
  |]
genFromLazyAvro namespaceBehavior (S.Record n _ _ _ fs) =
  [d| instance FromLazyAvro $(conT $ mkDataTypeName namespaceBehavior n) where
        fromLazyAvro (LV.Record _ r) =
           $(genFromLazyAvroFieldsExp (mkDataTypeName namespaceBehavior n) fs) r
        fromLazyAvro value           = $( [|\v -> badValue v $(mkTextLit $ S.renderFullname n)|] ) value
  |]
genFromLazyAvro namespaceBehavior (S.Fixed n _ s) =
  [d| instance FromLazyAvro $(conT $ mkDataTypeName namespaceBehavior n) where
        fromLazyAvro (LV.Fixed _ v)
          | BS.length v == s = pure $ $(conE (mkDataTypeName namespaceBehavior n)) v
        fromLazyAvro value = $( [|\v -> badValue v $(mkTextLit $ S.renderFullname n)|] ) value
  |]
genFromLazyAvro _ _                             = pure []

genFromLazyAvroFieldsExp :: Name -> [Field] -> Q Exp
genFromLazyAvroFieldsExp n []     = [| (return . return) $(conE n) |]
genFromLazyAvroFieldsExp n (x:xs) =
  [| \r ->
    $(let extract fld = [| r .~: T.pack $(mkTextLit (fldName fld))|]
          ctor = [| $(conE n) <$> $(extract x) |]
      in foldl (\expr fld -> [| $expr <*> $(extract fld) |]) ctor xs
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

------------------------- ToAvro ----------------------------------------------

genToAvro :: DeriveOptions -> Schema -> Q [Dec]
genToAvro opts s@(S.Enum n _ _ vs _) =
  toAvroInstance (mkSchemaValueName (namespaceBehavior opts) n)
  where
    conP' = flip conP [] . mkAdtCtorName (namespaceBehavior opts) n
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName (namespaceBehavior opts) n) where
            toAvro = $([| \x ->
              let convert = AT.Enum $(varE sname) (fromEnum $([|x|]))
              in $(caseE [|x|] ((\v -> match (conP' v)
                               (normalB [| convert (T.pack $(mkTextLit v))|]) []) <$> vs))
              |])
      |]
genToAvro opts s@(S.Record n _ _ _ fs) =
  toAvroInstance (mkSchemaValueName (namespaceBehavior opts) n)
  where
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName (namespaceBehavior opts) n) where
            toAvro = $(genToAvroFieldsExp sname)
      |]
    genToAvroFieldsExp sname = do
      names <- newNames "p_" (length fs)
      let con = conP (mkDataTypeName (namespaceBehavior opts) n) (varP <$> names)
      lamE [con]
            [| record $(varE sname)
                $(let assign (fld, n) = [| T.pack $(mkTextLit (fldName fld)) .= $(varE n) |]
                  in listE $ assign <$> zip fs names
                )
            |]

genToAvro opts s@(S.Fixed n _ size) =
  toAvroInstance (mkSchemaValueName (namespaceBehavior opts) n)
  where
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName (namespaceBehavior opts) n) where
            toAvro = $(do
              x <- newName "x"
              lamE [conP (mkDataTypeName (namespaceBehavior opts) n) [varP x]] [| AT.Fixed $(varE sname) $(varE x) |])
      |]

schemaDef :: Name -> Schema -> Q [Dec]
schemaDef sname sch = setName sname $
  [d|
      x :: Schema
      x = $(schemaDef' sch)
  |]

schemaDef' :: S.Type -> ExpQ
schemaDef' = mkSchema
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
          Union {..}     -> [e| Union $(mkV options) |]
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
          AT.Union ts t v -> [e| AT.Union $(mkV ts) $(mkSchema t) $(mkDefaultValue v) |]
          AT.Fixed s bs   -> [e| AT.Fixed $(mkSchema s) $(mkByteString bs) |]
          AT.Enum s n sym -> [e| AT.Enum $(mkSchema s) $(litE $ IntegerL $ fromIntegral n) $(mkText sym) |]

        mkByteString bs = [e| B.pack $(ListE <$> mapM numericLit (B.unpack bs)) |]
          where numericLit = litE . IntegerL . fromIntegral

        mkMap (HM.toList -> xs) = [e| HM.fromList $(ListE <$> mapM mkKVPair xs) |]
        mkKVPair (k, v)         = [e| ($(mkText k), $(mkDefaultValue v)) |]

        mkV (V.toList -> xs) = [e| V.fromList $(ListE <$> mapM mkSchema xs) |]

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
genType opts (S.Enum n _ _ vs _) = do
  let dname = mkDataTypeName (namespaceBehavior opts) n
  sequenceA [genEnum dname (mkAdtCtorName (namespaceBehavior opts) n <$> vs)]
genType opts (S.Fixed n _ s) = do
  let dname = mkDataTypeName (namespaceBehavior opts) n
  sequenceA [genNewtype dname]
genType _ _ = pure []

mkFieldTypeName :: NamespaceBehavior -> S.Type -> Q TH.Type
mkFieldTypeName namespaceBehavior = \case
  S.Boolean          -> [t| Bool |]
  S.Long             -> [t| Int64 |]
  S.Int              -> [t| Int32 |]
  S.Float            -> [t| Float |]
  S.Double           -> [t| Double |]
  S.Bytes            -> [t| ByteString |]
  S.String           -> [t| Text |]
  S.Union branches   -> union (V.toList branches)
  S.Record n _ _ _ _ -> [t| $(conT $ mkDataTypeName namespaceBehavior n) |]
  S.Map x            -> [t| Map Text $(go x) |]
  S.Array x          -> [t| [$(go x)] |]
  S.NamedType n      -> [t| $(conT $ mkDataTypeName namespaceBehavior n)|]
  S.Fixed n _ _      -> [t| $(conT $ mkDataTypeName namespaceBehavior n)|]
  S.Enum n _ _ _ _   -> [t| $(conT $ mkDataTypeName namespaceBehavior n)|]
  t                  -> error $ "Avro type is not supported: " <> show t
  where go = mkFieldTypeName namespaceBehavior
        union = \case
          [Null, x]       -> [t| Maybe $(go x) |]
          [x, Null]       -> [t| Maybe $(go x) |]
          [x, y]          -> [t| Either $(go x) $(go y) |]
          [a, b, c]       -> [t| Either3 $(go a) $(go b) $(go c) |]
          [a, b, c, d]    -> [t| Either4 $(go a) $(go b) $(go c) $(go d) |]
          [a, b, c, d, e] -> [t| Either5 $(go a) $(go b) $(go c) $(go d) $(go e) |]
          _               ->
            error "Unions with more than 5 elements are not yet supported"

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
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|], [t|Generic|]]
  pure $ DataD [] dn [] Nothing ((\n -> NormalC n []) <$> vs) [DerivClause Nothing ders]
#elif MIN_VERSION_template_haskell(2,11,0)
genEnum dn vs = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|], [t|Generic|]]
  pure $ DataD [] dn [] Nothing ((\n -> NormalC n []) <$> vs) ders
#else
genEnum dn vs = do
  [ConT eq, ConT sh, ConT or, ConT en, ConT gen] <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|], [t|Generic|]]
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
