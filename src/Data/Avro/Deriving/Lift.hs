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
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH

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
