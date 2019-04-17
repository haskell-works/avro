module Data.Avro.Decode.Lazy.LazyValue
where

import Data.ByteString
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.List.NonEmpty  (NonEmpty)
import Data.Text
import Data.Vector

data LazyValue f
      = Null
      | Boolean Bool
      | Int Int32
      | Long Int64
      | Float Float
      | Double Double
      | Bytes ByteString
      | String Text
      | Array (Vector (LazyValue f))            -- ^ Dynamically enforced monomorphic type.
      | Map (HashMap Text (LazyValue f))        -- ^ Dynamically enforced monomorphic type
      | Record f (HashMap Text (LazyValue f))   -- ^ Order and a map
      | Union (Vector f) f (LazyValue f)        -- ^ Set of union options, schema for selected option, and the actual value.
      | Fixed f ByteString
      | Enum f Int Text                         -- ^ An enum is a set of the possible symbols (the schema) and the selected symbol
      | Error !String
  deriving (Eq, Show)
