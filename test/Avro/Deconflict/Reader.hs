{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Avro.Deconflict.Reader
where

import           Data.Avro.Deconflict
import           Data.Avro.Deriving

deriveAvro "test/data/deconflict/reader.avsc"
