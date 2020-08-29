{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Avro.Decode.RawValuesSpec
where

import Avro.Data.Endpoint
import Data.Avro                   (decodeValueWithSchema, encodeContainerWithSchema, extractContainerValuesBytes, nullCodec)
import Data.Avro.Schema.ReadSchema (fromSchema)
import Data.Either                 (isLeft, isRight, rights)
import Data.List                   (unfoldr)
import Data.Text                   (pack)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Hedgehog.Range              (Range)
import Test.Hspec

import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Avro.Decode.RawValuesSpec" $ do

  it "should decode empty container" $ require $ withTests 1 $ property $ do
    empty <- evalIO $ encodeContainerWithSchema @Endpoint nullCodec schema'Endpoint []
    decoded <- evalEither $ extractContainerValuesBytes empty
    decoded === (schema'Endpoint, [])

  it "should decode container with one block" $ require $ withTests 5 $ property $ do
    msgs <- forAll $ Gen.list (Range.linear 1 3) endpointGen
    container   <- evalIO $ encodeContainerWithSchema nullCodec schema'Endpoint [msgs]
    (sch, vals) <- evalEither $ extractContainerValuesBytes container

    sch === schema'Endpoint
    let readSchema = fromSchema schema'Endpoint
    results <- evalEither $ traverse (decodeValueWithSchema readSchema) (rights vals)
    results === msgs


  it "should decode container with multiple blocks" $ require $ withTests 20 $ property $ do
    msgs <- forAll $ Gen.list (Range.linear 1 20) endpointGen
    container   <- evalIO $ encodeContainerWithSchema nullCodec schema'Endpoint (chunksOf 4 msgs)
    (sch, vals) <- evalEither $ extractContainerValuesBytes container

    sch === schema'Endpoint
    let readSchema = fromSchema schema'Endpoint
    results <- evalEither $ traverse (decodeValueWithSchema readSchema) (rights vals)
    results === msgs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not.null) . unfoldr (Just . splitAt n)
