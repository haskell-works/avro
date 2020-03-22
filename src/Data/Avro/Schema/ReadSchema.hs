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

-- | How to decode a value of target type @Long@.
-- This type controls how many bits are needed to be read from the encoded bytestring.
-- The number of bits can be different depending on differences between reader and writer schemas.
--
-- The rules are described in <https://avro.apache.org/docs/current/spec.html#Schema+Resolution>
data ReadLong
  = LongFromInt -- ^ Read @Int@ (32 bits) and cast it to @Long@ (Rule: int is promotable to long, float, or double)
  | ReadLong    -- ^ Read @Long@ (64 bits) and use as is
  deriving (Show, Eq, Ord, Generic, NFData)

-- | How to decode a value of target type @Float@.
-- This type controls how many bits are needed to be read from the encoded bytestring.
-- The number of bits can be different depending on differences between reader and writer schemas.
--
-- The rules are described in <https://avro.apache.org/docs/current/spec.html#Schema+Resolution>
data ReadFloat
  = FloatFromInt    -- ^ Read @Int@ (32 bits) and cast it to @Float@
  | FloatFromLong   -- ^ Read @Long@ (64 bits) and cast it to @Float@ (Rule: long is promotable to float or double)
  | ReadFloat       -- ^ Read @Float@ and use as is
  deriving (Show, Eq, Ord, Generic, NFData)

-- | How to decode a value of target type @Double@.
-- This type controls how many bits are needed to be read from the encoded bytestring.
-- The number of bits can be different depending on differences between reader and writer schemas.
--
-- The rules are described in <https://avro.apache.org/docs/current/spec.html#Schema+Resolution>
data ReadDouble
  = DoubleFromInt     -- ^ Read @Int@ (32 bits) and cast it to @Double@ (Rule: int is promotable to long, float, or double)
  | DoubleFromFloat   -- ^ Read @Float@ (64 bits) and cast it to @Double@ (Rule: float is promotable to float or double)
  | DoubleFromLong    -- ^ Read @Long@ (64 bits) and cast it to @Double@ (Rule: long is promotable to float or double)
  | ReadDouble
  deriving (Show, Eq, Ord, Generic, NFData)

-- | This type represents a /deconflicted/ version of a 'Schema'.
-- Schema resolution is described in Avro specification: <https://avro.apache.org/docs/current/spec.html#Schema+Resolution>
--
-- This library represents "normal" schema and "deconflicted" schema as different types to avoid confusion
-- between these two usecases (we shouldn't serialise values with such schema) and to be able to accomodate
-- some extra information that links between how data is supposed transformed between what reader wants
-- and what writer has.
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
              -- ^ Order of values represents order in the writer schema, an index represents order in a reader schema
              }
      | Fixed { name         :: TypeName
              , aliases      :: [TypeName]
              , size         :: Int
              , logicalTypeF :: Maybe LogicalTypeFixed
              }
      | FreeUnion { pos :: Int, ty :: ReadSchema }
    deriving (Eq, Show, Generic, NFData)

-- | Depending on differences between reader and writer schemas,
-- a record field can be found:
--
-- * Present in the reader schema but missing from the writer schema.
-- In this case the reader field is marked as 'Defaulted' with the
-- default value from the reader schema. An index value represents
-- the position of the field in the reader schema.
--
-- * Present in the writer schema but missing from the reader schema.
-- In this case the record field is marked as 'Ignored': the corresponding
-- bytes still need to be read from the payload (to advance the position in a bytestring),
-- but the result is discarded.
--
-- * Present in both reader and writer schemas.
-- In this case the field is marked to be read 'AsIs' with an index that
-- represents the field's position in the reader schema.
data FieldStatus
  = AsIs Int
  | Ignored
  | Defaulted Int S.DefaultValue
  deriving (Show, Eq, Ord, Generic, NFData)

-- | Deconflicted record field.
data ReadField = ReadField
  { fldName    :: Text
  , fldAliases :: [Text]
  , fldDoc     :: Maybe Text
  , fldOrder   :: Maybe Order
  , fldStatus  :: FieldStatus           -- ^ How the value of this field should be treated. See 'FieldStatus' documentation.
  , fldType    :: ReadSchema
  , fldDefault :: Maybe S.DefaultValue
  }
  deriving (Eq, Show, Generic, NFData)

-- | Converts Avro Schema to ReaderSchema trivially.
-- This function is useful when no deconflicting is required.
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
  Union{..}    -> HashMap.unions $ V.toList $ extractBindings . snd <$> options
  f@Fixed{..}  -> HashMap.fromList $ (name : aliases) `zip` repeat f
  Array{..}    -> extractBindings item
  Map{..}      -> extractBindings values
  _            -> HashMap.empty
