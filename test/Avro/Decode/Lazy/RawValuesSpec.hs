{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.Decode.Lazy.RawValuesSpec
where

import Data.Avro                     as A
import Data.Avro.Decode.Lazy         as DL
import Data.Avro.Decode.Lazy.Convert as TC
import Data.Avro.Deriving
import Data.Avro.Encode              (packContainerBlocks, packContainerValues)
import Data.Avro.Schema              (resultToEither)
import Data.Either                   (isLeft, isRight, rights)
import Data.List                     (unfoldr)
import Data.Semigroup                ((<>))
import Data.Text                     (pack)

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/small.avsc"

spec :: Spec
spec = describe "Avro.Decode.Lazy.RawValuesSpec" $ do

  it "should decode empty container" $ do
    empty <- A.encodeContainer ([] :: [[Endpoint]])
    DL.getContainerValuesBytes empty `shouldBe` Right (schema'Endpoint, [])

  it "should decode container with one block" $ do
    let msgs = mkEndpoint <$> [1, 2]
    container <- A.encodeContainer [msgs]
    let Right (sch, vals) = DL.getContainerValuesBytes container
    sch `shouldBe` schema'Endpoint
    let results = resultToEither . A.decode <$> rights vals
    rights results `shouldBe` msgs


  it "should decode container with multiple blocks" $ do
    let msgs = mkEndpoint <$> [1..19]
    container <- A.encodeContainer (chunksOf 4 msgs)
    let Right (sch, vals) = DL.getContainerValuesBytes container
    sch `shouldBe` schema'Endpoint
    let results = resultToEither . A.decode <$> rights vals
    rights results `shouldBe` msgs

mkEndpoint :: Int -> Endpoint
mkEndpoint i =
  Endpoint
    { endpointIps         = ["192.168.1." <> pack (show i), "127.0.0." <> pack (show i)]
    , endpointPorts       = [PortRange 1 10, PortRange 11 20]
    , endpointOpaque      = Opaque "16-b-long-string"
    , endpointCorrelation = Opaque "opaq-correlation"
    , endpointTag         = Left (fromIntegral i)
    }

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not.null) . unfoldr (Just . splitAt n)
