{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Bench.Deconflict.Writer
where

import Data.Avro.Deriving
import Data.Avro.Encoding.FromEncoding (getValue)
import Data.Avro.Encoding.Value        (FromValue (..), Value (..))
import Text.RawString.QQ

import           Control.Monad       (replicateM, when)
import           Data.Avro.Schema    (Field, Schema, TypeName)
import           Data.Binary.Get
import           Data.ByteString     (ByteString)
import           Data.Foldable
import           Data.Text           (Text)
import           Data.Traversable
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import           GHC.Int             (Int32, Int64)

import qualified Data.Avro.Decode.Get as Get
import qualified Data.Avro.Schema     as S
import qualified Data.Binary.Get      as Get

import qualified Data.ByteString.Lazy as LBS
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text

import Control.Monad.ST (ST)
import Data.Dynamic

import Control.DeepSeq

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "Outer",
  "fields": [
    { "name": "name", "type": "string" },
    { "name": "inner", "type": {
        "type": "record",
        "name": "Inner",
        "fields": [
          { "name": "id", "type": "int" }
        ]
      }
    },
    { "name": "other", "type": "Inner" }
  ]
}
|]

deriving instance NFData Inner
deriving instance NFData Outer

