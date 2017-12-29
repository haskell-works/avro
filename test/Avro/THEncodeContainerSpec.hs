{-# LANGUAGE TemplateHaskell #-}
module Avro.THEncodeContainerSpec where

import Data.Avro
import Data.Avro.Deriving

import Test.Hspec

import Control.Monad (void)
import Control.Exception

deriveAvro "test/data/record.avsc"

spec :: Spec
spec = describe "Avro.EncodeContainerSpec" $
  it "should encode data to a container of bytes" $
    (encodeContainer [[Thing 1]] >>= void . evaluate) `shouldReturn` ()
