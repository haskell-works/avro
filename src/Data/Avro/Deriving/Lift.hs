{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Avro.Deriving.Lift where

import qualified Data.Avro.Schema.Schema    as Schema
import qualified Data.ByteString            as ByteString
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text                  as Text
import qualified Data.Vector                as Vector
import           Language.Haskell.TH.Syntax (Lift (..))

#if MIN_VERSION_bytestring(0,11,2)
#else
instance Lift ByteString.ByteString where
  lift b = [| ByteString.pack $(lift $ ByteString.unpack b) |]
#endif

#if MIN_VERSION_text(1,2,4)
#else
instance Lift Text.Text where
  lift t = [| Text.pack $(lift $ Text.unpack t) |]
#endif

instance Lift a => Lift (Vector.Vector a) where
  lift v = [| Vector.fromList $(lift $ Vector.toList v) |]

#if MIN_VERSION_unordered_containers(0,2,17)
#else
instance (Lift k, Lift v) => Lift (HashMap.HashMap k v) where
  lift m = [| HashMap.fromList $(lift $ HashMap.toList m) |]
#endif

deriving instance Lift Schema.DefaultValue
deriving instance Lift Schema.Field
deriving instance Lift Schema.Order
deriving instance Lift Schema.TypeName
deriving instance Lift Schema.Decimal
deriving instance Lift Schema.LogicalTypeBytes
deriving instance Lift Schema.LogicalTypeFixed
deriving instance Lift Schema.LogicalTypeInt
deriving instance Lift Schema.LogicalTypeLong
deriving instance Lift Schema.LogicalTypeString
deriving instance Lift Schema.Schema
