-- | Avro encoding and decoding routines.
module Data.Avro
  ( FromAvro(..)
  , ToAvro(..)
  , Result(..)
  )

import Data.Avro.Encode as E
import Data.Avro.Decode as D
import Data.Avro.Schema as S
import Data.Avro.Types  as T

data Result a = Success a | Failure Text
  deriving (Eq,Ord,Show)

class FromAvro a where
  fromAvro :: T.Avro -> Result a

class ToAvro a where
  toAvro :: a -> T.Avro
