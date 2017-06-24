{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THSimpleSpec
where

import           Data.Avro
import           Data.Avro.TH

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/small.avsc"

spec :: Spec
spec = describe "Avro.THSpec: Small Schema" $ do
  it "should do roundtrip" $ do
    let msg = Endpoint
              { endpointIps   = ["192.168.1.1", "127.0.0.1"]
              , endpointPorts = [PortRange 1 10, PortRange 11 20]
              }
    fromAvro (toAvro msg) `shouldBe` pure msg

