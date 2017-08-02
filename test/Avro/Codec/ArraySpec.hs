{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.ArraySpec (spec) where

import Data.Avro
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text as T

import Test.Hspec
import qualified Test.QuickCheck as Q

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.Codec.ArraySpec" $ do
  it "list roundtip" $ Q.property $ \(xs :: [Int]) -> decode (schemaOf xs) (encode xs) == Success xs

  it "map roundtrip" $ Q.property $ \(xs :: Map String Int) ->
    let xs' = M.mapKeys T.pack xs
    in decode (schemaOf xs') (encode xs') == Success xs'
