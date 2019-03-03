{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.Decode.Lazy.ContainerSpec
where

import           Data.Avro                     as A
import           Data.Avro.Codec               as A
import           Data.Avro.Decode.Lazy         as DL
import           Data.Avro.Decode.Lazy.Convert as TC
import           Data.Avro.Deriving
import           Data.Avro.Encode              as E
import           Data.Either                   (isLeft)
import           Data.List                     (unfoldr)
import           Data.Semigroup                ((<>))
import           Data.Text                     (pack)
import Data.ByteString.Char8 (unpack)

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/small.avsc"

spec :: Spec
spec = do
  containerSpec A.nullCodec
  containerSpec A.deflateCodec

containerSpec :: A.Codec -> Spec
containerSpec codec = describe title $ do

  it "should decode empty container" $
    encodeThenDecode codec ([] :: [[Endpoint]]) >>= (`shouldBe` [])

  it "should decode container with one block" $ do
    let msg = mkEndpoint 1
    res <- encodeThenDecode codec [[msg]]
    sequence res `shouldBe` Right [msg]

  it "should decode container with empty blocks" $ do
    let msg = mkEndpoint 1
    res <- encodeThenDecode codec [[msg], [], []]
    sequence res `shouldBe` Right [msg]

  it "should decode container with empty blocks in between" $ do
    let (msg1, msg2) = (mkEndpoint 1, mkEndpoint 2)
    res <- encodeThenDecode codec [[msg1], [], [], [msg2]]
    sequence res `shouldBe` Right [msg1, msg2]

  it "should decode container with multiple blocks" $ do
    let msgs = mkEndpoint <$> [1..10]
    let chunks = chunksOf 4 msgs
    res <- encodeThenDecode codec chunks
    sequence res `shouldBe` Right msgs
  where
    title =
      "Avro.Decode.Lazy.ContainerSpec (" ++ unpack (A.codecName codec) ++ ")"


encodeThenDecode :: forall a. (FromLazyAvro a, ToAvro a) => A.Codec -> [[a]] -> IO [Either String a]
encodeThenDecode codec as =
  DL.decodeContainer <$>
    E.encodeContainer codec (schemaOf (undefined :: a)) (fmap (fmap toAvro) as)

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
