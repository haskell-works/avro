{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Avro.Schema.ReadSchema
( ReadSchema(..), ReadField(..)

, ReadLong(..), ReadFloat(..), ReadDouble(..)
, fromSchema, fromField

, extractBindings

, S.Decimal(..)
, S.LogicalTypeBytes(..), S.LogicalTypeFixed(..)
, S.LogicalTypeInt(..), S.LogicalTypeLong(..)
, S.LogicalTypeString(..)
, FieldStatus(..)
)
where

import           Control.DeepSeq         (NFData)
import           Data.Avro.Schema.Schema (LogicalTypeBytes, LogicalTypeFixed, LogicalTypeInt, LogicalTypeLong, LogicalTypeString, Order, TypeName)
import qualified Data.Avro.Schema.Schema as S
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           GHC.Generics            (Generic)

data ReadLong
  = LongFromInt
  | ReadLong
  deriving (Show, Eq, Ord, Generic, NFData)

data ReadFloat
  = FloatFromInt
  | FloatFromLong
  | ReadFloat
  deriving (Show, Eq, Ord, Generic, NFData)

data ReadDouble
  = DoubleFromInt
  | DoubleFromFloat
  | DoubleFromLong
  | ReadDouble
  deriving (Show, Eq, Ord, Generic, NFData)

-- | N.B. It is possible to create a Haskell value (of 'Schema' type) that is
-- not a valid Avro schema by violating one of the above or one of the
-- conditions called out in 'validateSchema'.
data ReadSchema
      =
      -- Basic types
        Null
      | Boolean
      | Int    { logicalTypeI :: Maybe LogicalTypeInt }
      | Long   { longReadFrom :: ReadLong, logicalTypeL :: Maybe LogicalTypeLong }
      | Float  { floatReadFrom :: ReadFloat }
      | Double { doubleReadFrom :: ReadDouble }
      | Bytes  { logicalTypeB :: Maybe LogicalTypeBytes }
      | String { logicalTypeS :: Maybe LogicalTypeString }
      | Array  { item :: ReadSchema }
      | Map    { values :: ReadSchema }
      | NamedType TypeName
      -- Declared types
      | Record { name    :: TypeName
               , aliases :: [TypeName]
               , doc     :: Maybe Text
               , order   :: Maybe Order
               , fields  :: [ReadField]
               }
      | Enum { name    :: TypeName
             , aliases :: [TypeName]
             , doc     :: Maybe Text
             , symbols :: V.Vector Text
             }
      | Union { options      :: V.Vector (Int, ReadSchema)
              }
      | Fixed { name         :: TypeName
              , aliases      :: [TypeName]
              , size         :: Int
              , logicalTypeF :: Maybe LogicalTypeFixed
              }
      | FreeUnion { pos :: Int, ty :: ReadSchema }
    deriving (Eq, Show, Generic, NFData)

data FieldStatus
  = AsIs Int
  | Ignored
  | Defaulted Int S.DefaultValue
  deriving (Show, Eq, Ord, Generic, NFData)


data ReadField = ReadField
  { fldName    :: Text
  , fldAliases :: [Text]
  , fldDoc     :: Maybe Text
  , fldOrder   :: Maybe Order
  , fldStatus  :: FieldStatus
  , fldType    :: ReadSchema
  , fldDefault :: Maybe S.DefaultValue
  }
  deriving (Eq, Show, Generic, NFData)

fromSchema :: S.Schema -> ReadSchema
fromSchema = \case
  S.Null        -> Null
  S.Boolean     -> Boolean
  S.Int l       -> Int l
  S.Long l      -> Long ReadLong l
  S.Float       -> Float ReadFloat
  S.Double      -> Double ReadDouble
  S.Bytes l     -> Bytes l
  S.String l    -> String l
  S.Array vs    -> Array $ fromSchema vs
  S.Map vs      -> Map $ fromSchema vs
  S.NamedType v -> NamedType v
  v@S.Record{}  -> Record
    { name    = S.name v
    , aliases = S.aliases v
    , doc     = S.doc v
    , order   = S.order v
    , fields  = (\(i, x) -> fromField (AsIs i) x) <$> zip [0..] (S.fields v)
    }
  v@S.Enum{} -> Enum
    { name    = S.name v
    , aliases = S.aliases v
    , doc     = S.doc v
    , symbols = S.symbols v
    }
  S.Union vs  -> Union . V.indexed $ fromSchema <$> vs
  v@S.Fixed{} -> Fixed
    { name          = S.name v
    , aliases       = S.aliases v
    , size          = S.size v
    , logicalTypeF  = S.logicalTypeF v
    }

fromField :: FieldStatus -> S.Field -> ReadField
fromField s v = ReadField
  { fldName     = S.fldName v
  , fldAliases  = S.fldAliases v
  , fldDoc      = S.fldDoc v
  , fldOrder    = S.fldOrder v
  , fldStatus   = s
  , fldType     = fromSchema (S.fldType v)
  , fldDefault  = S.fldDefault v
  }

-- | @extractBindings schema@ traverses a schema and builds a map of all declared
-- types.
--
-- Types declared implicitly in record field definitions are also included. No distinction
-- is made between aliases and normal names.
extractBindings :: ReadSchema -> HashMap.HashMap TypeName ReadSchema
extractBindings = \case
  t@Record{..} ->
    let withRecord = HashMap.fromList $ (name : aliases) `zip` repeat t
    in HashMap.unions $ withRecord : (extractBindings . fldType <$> fields)
  e@Enum{..}   -> HashMap.fromList $ (name : aliases) `zip` repeat e
  Union{..}    -> HashMap.unions $ V.toList $ extractBindings <$> snd <$> options
  f@Fixed{..}  -> HashMap.fromList $ (name : aliases) `zip` repeat f
  Array{..}    -> extractBindings item
  Map{..}      -> extractBindings values
  _            -> HashMap.empty
