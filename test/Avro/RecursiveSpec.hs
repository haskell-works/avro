{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Avro.RecursiveSpec
where

import Avro.Data.Recursive
import Data.Avro.Deriving

import Avro.TestUtils              (roundtripGen)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.RecursiveSpec" $ do
  it "should roundtrip recursive type" $ require $ property $
    roundtripGen schema'Recursive recursiveGen
