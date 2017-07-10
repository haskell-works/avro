{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.Avro.Deriving
( deriveAvro
, deriveAvro'
, deriveFromAvro
)
where

import           Control.Monad              (join)
import           Data.Aeson                 (eitherDecode)
import           Data.Char                  (isAlphaNum)
import qualified Data.Aeson                 as J
import           Data.Avro                  hiding (decode, encode)
import           Data.Avro.Schema           as S
import qualified Data.Avro.Types            as AT
import           Data.ByteString            (ByteString)
import           Data.Int
import           Data.List.NonEmpty         (NonEmpty( (:|) ))
import           Data.Map                   (Map)
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import           Language.Haskell.TH        as TH
import           Language.Haskell.TH.Syntax

import Data.Avro.Deriving.NormSchema

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import           Data.Text                  (Text)
import qualified Data.Text                  as T

-- | Derives Avro from a given schema file.
-- Generates data types, FromAvro and ToAvro instances.
deriveAvro :: FilePath -> Q [Dec]
deriveAvro p = readSchema p >>= deriveAvro'

deriveAvro' :: Schema -> Q [Dec]
deriveAvro' s = do
  let schemas = extractDerivables s
  types <- traverse genType schemas
  fromAvros <- traverse genFromAvro schemas
  toAvros <- traverse genToAvro schemas
  pure $ join types <> join fromAvros <> join toAvros

-- | Derives "read only" Avro from a given schema file.
-- Generates data types and FromAvro.
deriveFromAvro :: FilePath -> Q [Dec]
deriveFromAvro p = do
  schemas <- extractDerivables <$> readSchema p
  types <- traverse genType schemas
  fromAvros <- traverse genFromAvro schemas
  pure $ join types <> join fromAvros

readSchema :: FilePath -> Q Schema
readSchema p = do
  qAddDependentFile p
  mbSchema <- runIO $ decodeSchema p
  case mbSchema of
    Left err     -> fail $ "Unable to generate AVRO for " <> p <> ": " <> err
    Right sch    -> pure sch

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
genFromAvro _                             = pure []

genFromAvroFieldsExp :: Name -> [Field] -> Q Exp
genFromAvroFieldsExp n (x:xs) =
  [| \r ->
    $(let extract fld = [| r .: T.pack $(mkTextLit (fldName fld))|]
          ctor = [| $(conE n) <$> $(extract x) |]
      in foldl (\expr fld -> [| $expr <*> $(extract fld) |]) ctor xs
     )
  |]

genToAvro :: Schema -> Q [Dec]
genToAvro s@(Enum n _ _ _ vs _) = do
  let sname = mkSchemaValueName n
  sdef <- schemaDef sname s
  idef <- toAvroInstance sname
  pure (sdef <> idef)
  where
    conP' = flip conP [] . mkAdtCtorName n
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName n) where
            schema = pure $(varE sname)
            toAvro = $([| \x ->
              let convert = AT.Enum $(varE sname) (fromEnum $([|x|]))
              in $(caseE [|x|] ((\v -> match (conP' v)
                               (normalB [| convert (T.pack $(mkTextLit v))|]) []) <$> vs))
              |])
      |]

genToAvro s@(Record n _ _ _ _ fs) = do
  let sname = mkSchemaValueName n
  sdef <- schemaDef sname s
  idef <- toAvroInstance sname
  pure (sdef <> idef)
  where
    toAvroInstance sname =
      [d| instance ToAvro $(conT $ mkDataTypeName n) where
            toAvro = $(genToAvroFieldsExp sname)
            schema = pure $(varE sname)
      |]
    genToAvroFieldsExp sname = [| \r -> record $(varE sname)
        $(let assign fld = [| T.pack $(mkTextLit (fldName fld)) .= $(varE $ mkFieldTextName n fld) r |]
          in listE $ assign <$> fs
        )
      |]

schemaDef :: Name -> Schema -> Q [Dec]
schemaDef sname sch = setName sname $
  [d|
      x :: Schema
      x = fromMaybe undefined (J.decode (LBSC8.pack $(mkLit (LBSC8.unpack $ J.encode sch))))
  |]

-- | A hack around TemplateHaskell limitation:
-- It is currently not possible to splice variable name in QQ.
-- This function allows to replace hardcoded name into the specified one.
setName :: Name -> Q [Dec] -> Q [Dec]
setName = fmap . map . sn
  where
    sn n (SigD _ t) = SigD n t
    sn n (ValD (VarP _) x y) = ValD (VarP n) x y
    sn _ d = d

genType :: Schema -> Q [Dec]
genType (S.Record n _ _ _ _ fs) = do
  flds <- traverse (mkField n) fs
  let dname = mkDataTypeName n
  sequenceA [genDataType dname flds]
genType (S.Enum n _ _ _ vs _) = do
  let dname = mkDataTypeName n
  sequenceA [genEnum dname (mkAdtCtorName n <$> vs)]
genType _ = pure []

mkFieldTypeName :: S.Type -> Q TH.Type
mkFieldTypeName t = case t of
  S.Boolean                     -> [t| Bool |]
  S.Long                        -> [t| Int64 |]
  S.Int                         -> [t| Int |]
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

mkFieldTextName :: TypeName -> Field -> Name
mkFieldTextName (TN dn) fld = mkTextName . sanitiseName $
  updateFirst T.toLower dn <> updateFirst T.toUpper (fldName fld)

mkField :: TypeName -> Field -> Q VarStrictType
mkField prefix field = do
  ftype <- mkFieldTypeName (fldType field)
  let fName = mkFieldTextName prefix field
  pure (fName, defaultStrictness, ftype)

genEnum :: Name -> [Name] -> Q Dec
#if MIN_VERSION_template_haskell(2,11,0)
genEnum dn vs = do
  ders <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|]]
  pure $ DataD [] dn [] Nothing ((\n -> NormalC n []) <$> vs) ders
#else
genEnum dn vs = do
  [ConT eq, ConT sh, ConT or, ConT en] <- sequenceA [[t|Eq|], [t|Show|], [t|Ord|], [t|Enum|]]
  pure $ DataD [] dn [] ((\n -> NormalC n []) <$> vs) [eq, sh, or, en]
#endif

genDataType :: Name -> [VarStrictType] -> Q Dec
#if MIN_VERSION_template_haskell(2,11,0)
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
