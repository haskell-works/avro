{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Avro.Deconflict.A.Reader
where

import Data.Avro.Deconflict
import Data.Avro.Deriving

deriveAvro "test/data/deconflict/reader.avsc"

sampleValue :: Outer
sampleValue = Outer "Peone" (Inner 3 Nothing) (Inner 5 Nothing)
