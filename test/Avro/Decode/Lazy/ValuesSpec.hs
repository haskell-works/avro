{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.Decode.Lazy.ValuesSpec
where

import           Data.Avro
import           Data.Avro.Decode.Lazy
import           Data.Avro.Decode.Lazy.Convert as TC
import           Data.Avro.Deriving
import           Data.Either                   (isLeft)

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/small.avsc"

spec :: Spec
spec = describe "Avro.Decode.Lazy.ValueSpec" $ do
  let msg = Endpoint
              { endpointIps         = ["192.168.1.1", "127.0.0.1"]
              , endpointPorts       = [PortRange 1 10, PortRange 11 20]
              , endpointOpaque      = Opaque "16-b-long-string"
              , endpointCorrelation = Opaque "opaq-correlation"
              , endpointTag         = Left 14
              }

  it "should lazily decode correct value" $ do
    let lazyValue = decodeAvro schema'Endpoint (encode msg)
    TC.toStrictValue lazyValue `shouldBe` Right (toAvro msg)

  it "should return an error for a wrong content" $ do
    let lazyValue = decodeAvro schema'Endpoint "nonsense lives here"
    TC.toStrictValue lazyValue `shouldSatisfy` isLeft
