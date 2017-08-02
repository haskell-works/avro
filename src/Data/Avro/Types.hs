module Data.Avro.Types where

import Data.ByteString
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Text
import Data.Vector

data Value f
      = Null
      | Boolean !Bool
      | Int {-# UNPACK #-} !Int32
      | Long {-# UNPACK #-} !Int64
      | Float {-# UNPACK #-} !Float
      | Double {-# UNPACK #-} !Double
      | Bytes {-# UNPACK #-} !ByteString
      | String {-# UNPACK #-} !Text
      | Array (Vector (Value f))       -- ^ Dynamically enforced monomorphic type.
      | Map (HashMap Text (Value f))   -- ^ Dynamically enforced monomorphic type
      | Record f (HashMap Text (Value f)) -- Order and a map
      | Union (NonEmpty f) f (Value f) -- ^ Set of union options, schema for selected option, and the actual value.
      | Fixed {-# UNPACK #-} !ByteString
      | Enum f {-# UNPACK #-} !Int Text  -- ^ An enum is a set of the possible symbols (the schema) and the selected symbol
  deriving (Eq, Show)
