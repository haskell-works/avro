{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Avro.THEncodeContainerSpec where

import           Data.Avro
import           Data.Avro.Deriving

import           Test.Hspec

import           Control.Exception
import           Control.Monad      (void)

deriveAvro "test/data/record.avsc"

spec :: Spec
spec = describe "Avro.EncodeContainerSpec" $
  it "should encode data to a container of bytes" $
    (encodeContainer [[Thing 1]] >>= void . evaluate) `shouldReturn` ()
