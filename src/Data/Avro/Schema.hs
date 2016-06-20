{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
  ) where

import Data.Aeson hiding (Null,String,Array)
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser,typeMismatch)
import Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.HashMap.Strict as HMap
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import Data.Foldable as F
import Data.String
import Data.Monoid ((<>))
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.IO as T
import Data.Text.Encoding as T

-- |An Avro schema is either
-- * A "JSON object in the form `{"type":"typeName" ...` (DeclaredType that is not Union)
-- * A "JSON string, naming a defined type" (Basic Type w/o free variables/names)
-- * A "JSON array, representing a union" (DeclaredType w/ Union constr)
--
-- N.B. It is possible to create a Haskell value (of Schema type) that is
-- not a valid Avro schema by violating one of the above or one of the
-- conditions called out in 'validateSchema'.
data Schema = Schema Type

-- |Avro types are either primitive (string, int, etc)or declared
-- (structures, unions etc).
data Type u = BasicType BasicType | DeclaredType DeclaredType

newtype TypeName = TN T.Text
  deriving (Eq, Ord)

instance Show TypeName where
  show (TN s) = show s

instance IsString TypeName where
  fromString = TN . fromString

-- | An enumeration of the primitive types provided by Avro
data BasicType
      = Null
      | Boolean
      | Int   | Long
      | Float | Double
      | Bytes | String
      | Array { item :: TypeName }
      | Map   { values :: TypeName }
  deriving (Eq, Ord)

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
      | Union { options  :: [TypeName]
              }
      | Fixed { name      :: TypeName
              , namespace :: Maybe Text
              , aliases   :: [TypeName]
              , size      :: Integer
              }

data Field = Field { fldName       :: Text
                   , fldDoc        :: Maybe Text
                   , fldType       :: Type
                   , fldDefault    :: Maybe A.Value
                   , fldOrder      :: Maybe Order
                   , fldAliases    :: [Text]
                   }

data Order = Ascending | Descending | Ignore
  deriving (Eq, Ord)

instance FromJSON Schema where
  parseJSON val =
    case val of
      o@(Object _) -> Schema <$> parseJSON o
      A.Array arr  -> Schema . DeclaredType . Union . V.toList <$> mapM parseJSON arr

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
      _          -> fail $ "Unrecognized raw string in schema: " <> T.unpack s
  parseJSON (Object o) =
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
      Union  {..} -> A.Array $ V.fromList $ map toJSON options
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
  parseJSON (Object o) =
    Field <$> o .: "name"
          <*> o .:? "doc"
          <*> o .: "type"
          <*> o .:? "default"
          <*> o .:? ("order" :: Text)    .!= Just Ascending
          <*> o .:? ("aliases" :: Text)  .!= []
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

-- | validates a schema to ensure
--
--  * All types are defined
--  * Unions do not directly contain other unions
--  * Unions are not ambiguous (may not contain more than one schema with
--  the same type except for named types of record, fixed and enum)
--  * Default values for unions can be cast as the type indicated by the
--  first structure.
--  * Default values can be cast/de-serialize correctly.
validateSchema :: Schema -> Parser ()
validateSchema sch = return () -- XXX TODO

resolveSchema :: Schema -> Schema -> Either String Schema
resolveSchema (Schema e) (Schema d) = Schema <$> go e d
 where
 go :: Type -> Type -> Either String Type
 go (BasicType (Array xs)) (BasicType (Array ys)) = BasicType . Array <$> resolveArray xs ys
 go enc@(BasicType (Map aTy)) (BasicType (Map bTy))
      | aTy == bTy = Right enc
 go enc@(BasicType eTy) dec@(BasicType dTy)
      | eTy == dTy = Right enc
      | otherwise  = case eTy of
                      Int  | dTy `elem` [Long,Float,Double] -> Right dec
                      Long | dTy `elem` [Float,Double]      -> Right dec
                      Float | dTy == Double                 -> Right dec
                      String | dTy == Bytes                 -> Right dec
                      Bytes  | dTy == String                -> Right dec

 go (DeclaredType a@(Enum {})) (DeclaredType b@(Enum {}))
      | name a == name b = resolveEnum a b
 go (DeclaredType a@(Fixed {})) (DeclaredType b@(Fixed {}))
      | name a == name b && size a == size b = DeclaredType <$> resolveFixed a b
 go (DeclaredType a@(Record {})) (DeclaredType b@(Record {}))
      | name a == name b = DeclaredType <$> resolveRecord a b
 go (DeclaredType (Union xs)) (DeclaredType y) = DeclaredType <$> resolveUnion1 xs y
 go x (DeclaredType (Union ys)) = DeclaredType <$> resolveUnion2 x ys

 resolveArray = undefined
 resolveFixed = undefined
 resolveEnum = undefined
 resolveUnion1 = undefined
 resolveUnion2 = undefined
 resolveRecord :: DeclaredType -> DeclaredType -> Either String DeclaredType
 resolveRecord eRec dRec =
  case resolveFields (fields eRec) (fields dRec) of
    Left s     -> Left s
    Right flds -> Right dRec { fields = flds}

 resolveFields :: [Field] -> [Field] -> Either String [Field]
 resolveFields (ef:eFlds) (df:dFlds) = undefined

