{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Avro.Deriving.Lift
where

import qualified Data.Avro.Schema           as Schema
import qualified Data.Avro.Types.Value      as Avro
import qualified Data.ByteString            as ByteString
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text                  as Text
import qualified Data.Vector                as Vector
import           Language.Haskell.TH.Syntax (Lift (..))

instance Lift ByteString.ByteString where
  lift b = [| ByteString.pack $(lift $ ByteString.unpack b) |]

instance Lift Text.Text where
  lift t = [| Text.pack $(lift $ Text.unpack t) |]

instance Lift a => Lift (Vector.Vector a) where
  lift v = [| Vector.fromList $(lift $ Vector.toList v) |]

instance (Lift k, Lift v) => Lift (HashMap.HashMap k v) where
  lift m = [| HashMap.fromList $(lift $ HashMap.toList m) |]

deriving instance Lift f => Lift (Avro.Value f)
deriving instance Lift Schema.Field
deriving instance Lift Schema.Order
deriving instance Lift Schema.TypeName
deriving instance Lift Schema.Type
