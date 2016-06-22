module Data.Avro.Types where

import Data.Int
import Data.Text
import Data.Vector
import Data.ByteString
import Data.HashMap.Strict (HashMap)

type UntypedValue = Value ()
data Value f
      = Null
      | Boolean !Bool
      | Int !Int32
      | Long !Int64
      | Float !Float
      | Double !Double
      | Bytes !ByteString
      | String !Text
      | Array (Vector (Value f)) -- ^ Dynamically enforced monomorphic type.
      | Map (HashMap Text (Value f))   -- ^ Dynamically enforced monomorphic type
      | Record (HashMap Text (Value f))
      | Union f (Value f)
      | Fixed !ByteString
      | Enum !Text
  deriving (Eq, Show)
