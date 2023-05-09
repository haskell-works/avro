{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Avro.Deriving.Lift where

import qualified Data.Avro.Schema.Schema    as Schema

import           Language.Haskell.TH.Syntax (Lift (..))
import           Instances.TH.Lift ()

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
