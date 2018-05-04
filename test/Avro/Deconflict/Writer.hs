{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Avro.Deconflict.Writer
where

import           Data.Avro.Deconflict
import           Data.Avro.Deriving

deriveAvro "test/data/deconflict/writer.avsc"
