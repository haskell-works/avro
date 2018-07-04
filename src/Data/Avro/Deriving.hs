{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.Avro.Deriving
( -- * Deriving options
  DeriveOptions(..), defaultDeriveOptions
, mkPrefixedFieldName, mkAsIsFieldName

  -- * Deriving Haskell types from Avro schema
, deriveAvroWithOptions
, deriveAvroWithOptions'
, deriveFromAvroWithOptions
, deriveAvro
, deriveAvro'
, deriveFromAvro
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
import           GHC.Generics                  (Generic)
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

-- | Derives Avro from a given schema file.
-- Generates data types, FromAvro and ToAvro instances.
data DeriveOptions = DeriveOptions
  { -- | How to build field names for generated data types
    doFieldNameBuilder :: TypeName -> Field -> Name
  }

-- | Default deriving options
--
-- @
-- defaultDeriveOptions = 'DeriveOptions'
--   { doFieldNameBuilder = 'mkPrefixedFieldName'
--   }
-- @
defaultDeriveOptions = DeriveOptions
  { doFieldNameBuilder = mkPrefixedFieldName
  }

-- | Generates a field name that is prefixed with the type name.
--
-- For example, if the schema defines type 'Person' that has a field 'firstName',
-- then the generated Haskell type will be like
--
-- @
-- Person { personFirstName :: Text }
-- @
mkPrefixedFieldName :: TypeName -> Field -> Name
mkPrefixedFieldName (TN dn) fld = mkTextName . sanitiseName $
  updateFirst T.toLower dn <> updateFirst T.toUpper (fldName fld)

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

mkAsIsFieldName :: TypeName -> Field -> Name
mkAsIsFieldName _ = mkTextName . sanitiseName . updateFirst T.toLower . fldName

-- | Generates Haskell classes and 'FromAvro' and 'ToAvro' instances
-- given the Avro schema file
deriveAvroWithOptions :: DeriveOptions -> FilePath -> Q [Dec]
deriveAvroWithOptions o p = readSchema p >>= deriveAvroWithOptions' o

-- | Generates Haskell classes and 'FromAvro' and 'ToAvro' instances
-- given the Avro schema
deriveAvroWithOptions' :: DeriveOptions -> Schema -> Q [Dec]
deriveAvroWithOptions' o s = do
  let schemas = extractDerivables s
  types     <- traverse (genType o) schemas
  hasSchema <- traverse genHasAvroSchema schemas
  fromAvros <- traverse genFromAvro schemas
  toAvros   <- traverse (genToAvro o) schemas
  pure $ join types <> join hasSchema <> join fromAvros <> join toAvros

-- | Derives "read only" Avro from a given schema file.
-- Generates data types and FromAvro.
deriveFromAvroWithOptions :: DeriveOptions -> FilePath -> Q [Dec]
deriveFromAvroWithOptions o p = do
  schemas   <- extractDerivables <$> readSchema p
  types     <- traverse (genType o) schemas
  hasSchema <- traverse genHasAvroSchema schemas
  fromAvros <- traverse genFromAvro schemas
  pure $ join types <> join hasSchema <> join fromAvros

-- | Same as 'deriveAvroWithOptions' but uses 'defaultDeriveOptions'
--
-- @
-- deriveAvro' = deriveAvroWithOptions' 'defaultDeriveOptions'
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

-- | Derives "read only" Avro from a given schema file.
-- Generates data types and FromAvro.
deriveFromAvro :: FilePath -> Q [Dec]
deriveFromAvro = deriveFromAvroWithOptions defaultDeriveOptions

readSchema :: FilePath -> Q Schema
readSchema p = do
  qAddDependentFile p
  mbSchema <- runIO $ decodeSchema p
  case mbSchema of
    Left err  -> fail $ "Unable to generate AVRO for " <> p <> ": " <> err
    Right sch -> pure sch

genFromAvro :: Schema -> Q [Dec]
genFromAvro (S.Enum n _ _ _ _ _) =
  [d| instance FromAvro $(conT $ mkDataTypeName n) where
        fromAvro (AT.Enum _ i _) = $([| pure . toEnum|]) i
        fromAvro value           = $( [|\v -> badValue v $(mkTextLit $ unTN n)|] ) value
  |]
genFromAvro (S.Record n _ _ _ _ fs) =
  [d| instance FromAvro $(conT $ mkDataTypeName n) where
        fromAvro (AT.Record _ r) = $(genFromAvroFieldsExp (mkTextName $ unTN n) fs) r
        fromAvro value           = $( [|\v -> badValue v $(mkTextLit $ unTN n)|] ) value
  |]
genFromAvro (S.Fixed n _ _ s) =
  [d| instance FromAvro $(conT $ mkDataTypeName n) where
        fromAvro (AT.Fixed _ v) | BS.length v == s = pure $ $(conE (mkDataTypeName n)) v
        fromAvro value = $( [|\v -> badValue v $(mkTextLit $ unTN n)|] ) value
  |]
genFromAvro _                             = pure []

genFromAvroFieldsExp :: Name -> [Field] -> Q Exp
genFromAvroFieldsExp n (x:xs) =
  [| \r ->
    $(let extract fld = [| r .: T.pack $(mkTextLit (fldName fld))|]
          ctor = [| $(conE n) <$> $(extract x) |]
      in foldl (\expr fld -> [| $expr <*> $(extract fld) |]) ctor xs
     )
  |]

genHasAvroSchema :: Schema -> Q [Dec]
genHasAvroSchema s = do
  let sname = mkSchemaValueName (name s)
  sdef <- schemaDef sname s
  idef <- hasAvroSchema sname
  pure (sdef <> idef)
  where
    hasAvroSchema sname =
      [d| instance HasAvroSchema $(conT $ mkDataTypeName (name s)) where
            schema = pure $(varE sname)
      |]

genToAvro :: DeriveOptions -> Schema -> Q [Dec]
genToAvro opts s@(Enum n _ _ _ vs _) =
  toAvroInstance (mkSchemaValueName n)
  where
    conP' = flip conP [] . mkAdtCtorName n
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName n) where
            toAvro = $([| \x ->
              let convert = AT.Enum $(varE sname) (fromEnum $([|x|]))
              in $(caseE [|x|] ((\v -> match (conP' v)
                               (normalB [| convert (T.pack $(mkTextLit v))|]) []) <$> vs))
              |])
      |]

genToAvro opts s@(Record n _ _ _ _ fs) =
  toAvroInstance (mkSchemaValueName n)
  where
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName n) where
            toAvro = $(genToAvroFieldsExp sname)
      |]
    genToAvroFieldsExp sname = [| \r -> record $(varE sname)
        $(let assign fld = [| T.pack $(mkTextLit (fldName fld)) .= $(varE $ (doFieldNameBuilder opts) n fld) r |]
          in listE $ assign <$> fs
        )
      |]

genToAvro opts s@(Fixed n _ _ size) =
  toAvroInstance (mkSchemaValueName n)
  where
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName n) where
            toAvro = $(do
              x <- newName "x"
              lamE [conP (mkDataTypeName n) [varP x]] [| AT.Fixed $(varE sname) $(varE x) |])
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
                                       , namespace = $(mkMaybeText namespace)
                                       , aliases   = $(ListE <$> mapM mkName aliases)
                                       , doc       = $(mkMaybeText doc)
                                       , order     = $(mkOrder order)
                                       , fields    = $(ListE <$> mapM mkField fields)
                                       }
                              |]
          Enum {..}      -> [e| mkEnum $(mkName name)
                                       $(ListE <$> mapM mkName aliases)
                                       $(mkMaybeText namespace)
                                       $(mkMaybeText doc)
                                       $(ListE <$> mapM mkText symbols)
                              |]
          Union {..}     -> [e| mkUnion $(mkNE options) |]
          Fixed {..}     -> [e| Fixed { name      = $(mkName name)
                                      , namespace = $(mkMaybeText namespace)
                                      , aliases   = $(ListE <$> mapM mkName aliases)
                                      , size      = $(litE $ IntegerL $ fromIntegral size)
                                      }
                              |]

        mkText text = [e| T.pack $(mkTextLit text) |]

        mkName (TN name) = [e| TN $(mkText name) |]

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
          AT.Union ts t v -> [e| AT.Union $(mkNE ts) $(mkSchema t) $(mkDefaultValue v) |]
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

genType :: DeriveOptions -> Schema -> Q [Dec]
genType opts (S.Record n _ _ _ _ fs) = do
  flds <- traverse (mkField opts n) fs
  let dname = mkDataTypeName n
  sequenceA [genDataType dname flds]
genType _ (S.Enum n _ _ _ vs _) = do
  let dname = mkDataTypeName n
  sequenceA [genEnum dname (mkAdtCtorName n <$> vs)]
genType _ (S.Fixed n _ _ s) = do
  let dname = mkDataTypeName n
  sequenceA [genNewtype dname]

genType _ _ = pure []

mkFieldTypeName :: S.Type -> Q TH.Type
mkFieldTypeName t = case t of
  S.Boolean                     -> [t| Bool |]
  S.Long                        -> [t| Int64 |]
  S.Int                         -> [t| Int32 |]
  S.Float                       -> [t| Float |]
  S.Double                      -> [t| Double |]
  S.Bytes                       -> [t| ByteString |]
  S.String                      -> [t| Text |]
  S.Union (Null :| [x]) _       -> [t| Maybe $(mkFieldTypeName x) |] -- AppT (ConT $ mkName "Maybe") (mkFieldTypeName x)
  S.Union (x :| [Null]) _       -> [t| Maybe $(mkFieldTypeName x) |] --AppT (ConT $ mkName "Maybe") (mkFieldTypeName x)
  S.Union (x :| [y]) _          -> [t| Either $(mkFieldTypeName x) $(mkFieldTypeName y) |] -- AppT (AppT (ConT (mkName "Either")) (mkFieldTypeName x)) (mkFieldTypeName y)
  S.Union (_ :| _) _            -> error "Unions with more than 2 elements are not yet supported"
  S.Record n _ _ _ _ _          -> [t| $(conT $ mkDataTypeName n) |]
  S.Map x                       -> [t| Map Text $(mkFieldTypeName x) |] --AppT (AppT (ConT (mkName "Map")) (ConT $ mkName "Text")) (mkFieldTypeName x)
  S.Array x                     -> [t| [$(mkFieldTypeName x)] |]--AppT (ConT $ Text "[]") (mkFieldTypeName x)
  S.NamedType n                 -> [t| $(conT $ mkDataTypeName n)|] --ConT . mkName . T.unpack . mkDataTypeName $ x
  S.Fixed n _ _ _               -> [t| $(conT $ mkDataTypeName n)|] --ConT . mkName . T.unpack . mkDataTypeName $ x
  S.Enum n _ _ _ _ _            -> [t| $(conT $ mkDataTypeName n)|]
  _                             -> error $ "Avro type is not supported: " <> show t

updateFirst :: (Text -> Text) -> Text -> Text
updateFirst f t =
  let (l, ls) = T.splitAt 1 t
  in f l <> ls

decodeSchema :: FilePath -> IO (Either String Schema)
decodeSchema p = eitherDecode <$> LBS.readFile p

mkAdtCtorName :: TypeName -> Text -> Name
mkAdtCtorName prefix nm =
  concatNames (mkDataTypeName prefix) (mkDataTypeName' nm)

concatNames :: Name -> Name -> Name
concatNames a b = mkName $ nameBase a <> nameBase b

sanitiseName :: Text -> Text
sanitiseName =
  let valid c = isAlphaNum c || c == '\'' || c == '_'
  in T.concat . T.split (not . valid)

mkSchemaValueName :: TypeName -> Name
mkSchemaValueName (TN n) = mkTextName $ "schema'" <> n

mkDataTypeName :: TypeName -> Name
mkDataTypeName = mkDataTypeName' . unTN

mkDataTypeName' :: Text -> Name
mkDataTypeName' =
  mkTextName . sanitiseName . updateFirst T.toUpper . T.takeWhileEnd (/='.')

mkField :: DeriveOptions -> TypeName -> Field -> Q VarStrictType
mkField opts prefix field = do
  ftype <- mkFieldTypeName (fldType field)
  let fName = (doFieldNameBuilder opts) prefix field
  pure (fName, defaultStrictness, ftype)

genNewtype :: Name -> Q Dec
#if MIN_VERSION_template_haskell(2,12,0)
genNewtype dn = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
  fldType <- [t|ByteString|]
  let ctor = RecC dn [(mkName ("un" ++ nameBase dn), defaultStrictness, fldType)]
  pure $ NewtypeD [] dn [] Nothing ctor [DerivClause Nothing ders]
#elif MIN_VERSION_template_haskell(2,11,0)
genNewtype dn = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
  fldType <- [t|ByteString|]
  let ctor = RecC dn [(mkName ("un" ++ nameBase dn), defaultStrictness, fldType)]
  pure $ NewtypeD [] dn [] Nothing ctor ders
#else
genNewtype dn = do
  [ConT eq, ConT sh] <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
  fldType <- [t|ByteString|]
  let ctor = RecC dn [(mkName ("un" ++ nameBase dn), defaultStrictness, fldType)]
  pure $ NewtypeD [] dn [] ctor [eq, sh]
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
  [ConT eq, ConT sh, ConT or, ConT en] <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|], [t|Generic|]]
  pure $ DataD [] dn [] ((\n -> NormalC n []) <$> vs) [eq, sh, or, en]
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
  [ConT eq, ConT sh] <- sequenceA [[t|Eq|], [t|Show|], [t|Generic|]]
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
