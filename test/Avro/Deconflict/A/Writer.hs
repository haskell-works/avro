{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Avro.Deconflict.A.Writer
where

import Data.Avro.Deconflict
import Data.Avro.Deriving

deriveAvro "test/data/deconflict/writer.avsc"

sampleValue :: Outer
sampleValue = Outer "Peone" (Inner 3) (Inner 5)
