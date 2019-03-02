{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.Decode.Lazy.RawBlocksSpec
where

import Data.Avro                     as A
import Data.Avro.Codec               (nullCodec)
import Data.Avro.Decode.Lazy         as DL
import Data.Avro.Decode.Lazy.Convert as TC
import Data.Avro.Deriving
import Data.Avro.Encode              (packContainerBlocks, packContainerValues)
import Data.Either                   (isLeft, isRight, rights)
import Data.List                     (unfoldr)
import Data.Semigroup                ((<>))
import Data.Text                     (pack)

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/small.avsc"

spec :: Spec
spec = describe "Avro.Decode.Lazy.RawBlocksSpec" $ do

  it "should decode empty container" $ do
    empty <- A.encodeContainer ([] :: [[Endpoint]])
    DL.decodeRawBlocks empty `shouldBe` Right (schema'Endpoint, [])

  it "should decode container with one block" $ do
    container <- A.encodeContainer [mkEndpoint <$> [1, 2]]
    let Right (s, bs) = DL.decodeRawBlocks container
    s `shouldBe` schema'Endpoint
    fmap fst <$> bs `shouldBe` [Right 2]
    sequence bs `shouldSatisfy` isRight

  it "should decode container with multiple blocks" $ do
    container <- A.encodeContainer (chunksOf 4 $ mkEndpoint <$> [1..10])
    let Right (s, bs) = DL.decodeRawBlocks container
    s `shouldBe` schema'Endpoint
    fmap fst <$> bs `shouldBe` [Right 4, Right 4, Right 2]
    sequence bs `shouldSatisfy` isRight

  it "should repack container" $ do
    let srcValues = mkEndpoint <$> [1..19]
    srcContainer <- A.encodeContainer (chunksOf 4 srcValues)
    let Right (s, bs) = DL.decodeRawBlocks srcContainer
    tgtContainer <- packContainerBlocks nullCodec s (rights bs)
    let tgtValues = DL.decodeContainer tgtContainer
    let allTgtValues = rights tgtValues
    allTgtValues `shouldBe` srcValues

  it "should pack container with individual values" $ do
    let srcValues = mkEndpoint <$> [1..19]
    let values = A.encode <$> srcValues
    container <- packContainerValues nullCodec schema'Endpoint (chunksOf 4 values)

    let Right (s, bs) = DL.decodeRawBlocks container
    s `shouldBe` schema'Endpoint
    fst <$> rights bs `shouldBe` [4,4,4,4,3]

    rights (DL.decodeContainer container) `shouldBe` srcValues

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
