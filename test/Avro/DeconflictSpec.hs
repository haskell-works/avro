{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Avro.DeconflictSpec
where

import           Data.Avro              as A
import qualified Data.Avro.Decode       as A (decodeAvro)
import           Data.Avro.Deconflict
import           Data.Avro.Deriving
import           Data.Avro.Schema
import qualified Data.Avro.Types        as Ty
import           Data.Either
import           Data.List.NonEmpty     (NonEmpty (..))

import qualified Avro.Deconflict.Reader as R
import qualified Avro.Deconflict.Writer as W

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

writerMessage :: W.Outer
writerMessage = W.Outer "Peone" (W.Inner 3)

spec :: Spec
spec = describe "Avro.DeconflictSpec" $ do
  it "should deconflict simple message" $ do
    let payload = A.encode $ W.Inner 3
    let Right decodedAvro = A.decodeAvro W.schema'Inner payload
    let Right deconflicted = deconflict W.schema'Inner R.schema'Inner decodedAvro
    fromAvro deconflicted `shouldBe` Success (R.Inner 3 Nothing)

  it "should deconflict nested message" $ do
    let payload = A.encode writerMessage
    let Right decodedAvro = A.decodeAvro W.schema'Outer payload
    let Right deconflicted = deconflict W.schema'Outer R.schema'Outer decodedAvro

    fromAvro deconflicted `shouldBe` Success (R.Outer "Peone" (R.Inner 3 Nothing))


