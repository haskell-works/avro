{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THSimpleSpec
where

import           Control.Monad
import           Data.Aeson (decodeStrict)
import           Data.Avro
import           Data.Avro.Deriving
import           Data.Avro.Schema
import           Data.ByteString as BS

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/small.avsc"

spec :: Spec
spec = describe "Avro.THSpec: Small Schema" $ do
  let msgs =
        [ Endpoint
          { endpointIps   = ["192.168.1.1", "127.0.0.1"]
          , endpointPorts = [PortRange 1 10, PortRange 11 20]
          }
        , Endpoint
          { endpointIps   = []
          , endpointPorts = [PortRange 1 10, PortRange 11 20]
          }
        ]

  it "should do roundtrip" $ do
    forM_ msgs $ \msg ->
      fromAvro (toAvro msg) `shouldBe` pure msg

  it "should do full round trip" $
    forM_ msgs $ \msg -> do
      let encoded = encode msg
      let decoded = decode (schemaOf msg) encoded

      pure msg `shouldBe` decoded
