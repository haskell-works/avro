{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Decode.ContainerSpec
where

import Data.Avro.Codec       (Codec (..), deflateCodec, nullCodec)
import Data.ByteString.Char8 (unpack)
import Data.List             (unfoldr)

import           Avro.Data.Endpoint
import           Avro.TestUtils
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Hedgehog.Range              (Range)
import qualified Hedgehog.Range              as Range
import           Test.Hspec

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = do
  containerSpec nullCodec
  containerSpec deflateCodec

containerSpec :: Codec -> Spec
containerSpec codec = describe title $ do
  it "should decode empty container" $ require $ withTests 1 $ property $ do
    tripContainer []

  it "should decode container with one block" $ require $ property $ do
    msg <- forAll endpointGen
    tripContainer [[msg]]

  it "should decode container with empty blocks" $ require $ property $ do
    msg <- forAll endpointGen
    tripContainer [[msg], [], []]

  it "should decode container with empty blocks in between" $ require $ property $ do
    (msg1, msg2) <- forAll $ (,) <$> endpointGen <*> endpointGen
    tripContainer [[msg1], [], [], [msg2]]

  it "should decode container with multiple blocks" $ require $ property $ do
    msgs <- forAll $ Gen.list (Range.linear 1 10) endpointGen
    tripContainer (chunksOf 4 msgs)
  where
    tripContainer = roundtripContainer' codec schema'Endpoint
    title =
      "Avro.Decode.ContainerSpec (" ++ unpack (codecName codec) ++ ")"


chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not.null) . unfoldr (Just . splitAt n)
