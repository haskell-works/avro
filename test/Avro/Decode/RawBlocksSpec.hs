{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Avro.Decode.RawBlocksSpec
where

import Avro.Data.Endpoint
import Control.Monad                (forM_)
import Data.Avro                    (decodeContainerWithEmbeddedSchema, encodeContainerWithSchema, encodeValueWithSchema, nullCodec)
import Data.Avro.Internal.Container (decodeRawBlocks, packContainerBlocks, packContainerValues)
import Data.Either                  (rights)
import Data.List                    (unfoldr)
import Data.Semigroup               ((<>))
import Data.Text                    (pack)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Range              (Range)
import Test.Hspec

import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.Decode.RawBlocksSpec" $ do

  it "should decode empty container" $ require $ withTests 1 $ property $ do
    empty <- evalIO $ encodeContainerWithSchema @Endpoint nullCodec schema'Endpoint []
    decoded <- evalEither $ decodeRawBlocks empty
    decoded === (schema'Endpoint, [])

  it "should decode container with one block" $ require $ withTests 5 $ property $ do
    msgs <- forAll $ Gen.list (Range.linear 1 5) endpointGen
    container <- evalIO $ encodeContainerWithSchema nullCodec schema'Endpoint [msgs]
    (s, bs)   <- evalEither $ decodeRawBlocks container

    s === schema'Endpoint
    blocks <- evalEither $ sequence bs
    fmap fst blocks === [length msgs]

  it "should decode container with multiple blocks" $ require $ withTests 20 $ property $ do
    msgs <- forAll $ Gen.list (Range.linear 1 19) endpointGen
    container <- evalIO $ encodeContainerWithSchema nullCodec schema'Endpoint (chunksOf 4 msgs)
    (s, bs)   <- evalEither $ decodeRawBlocks container

    s === schema'Endpoint
    blocks <- evalEither $ sequence bs

    let blockLengths = fst <$> blocks
    sum blockLengths === length msgs
    diff (last blockLengths) (<=) 4
    assert $ all (==4) (init blockLengths)

  it "should repack container" $ require $ withTests 20 $ property $ do
    srcValues <- forAll $ Gen.list (Range.linear 1 19) endpointGen
    srcContainer  <- evalIO $ encodeContainerWithSchema nullCodec schema'Endpoint (chunksOf 4 srcValues)
    (s, bs)       <- evalEither $ decodeRawBlocks srcContainer

    tgtContainer <- evalIO $ packContainerBlocks nullCodec s (rights bs)
    tgtValues <- evalEither . sequence $ decodeContainerWithEmbeddedSchema tgtContainer

    tgtValues === srcValues

  it "should pack container with individual values" $ require $ withTests 20 $ property $ do
    srcValues <- forAll $ Gen.list (Range.linear 1 19) endpointGen
    let values = encodeValueWithSchema schema'Endpoint <$> srcValues
    container <- evalIO $ packContainerValues nullCodec schema'Endpoint (chunksOf 4 values)

    (s, bs)  <- evalEither $ decodeRawBlocks container
    s === schema'Endpoint

    blocks <- evalEither $ sequence bs
    let blockLengths = fst <$> blocks
    diff (last blockLengths) (<=) 4
    assert $ all (==4) (init blockLengths)

    tgtValues <- evalEither . sequence $ decodeContainerWithEmbeddedSchema container
    tgtValues === srcValues

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not.null) . unfoldr (Just . splitAt n)
