{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.DeconflictSpec where

import Data.Avro            as A
import Data.Avro.Deconflict
import Data.Avro.Deriving
import Data.Avro.Schema
import Data.Either
import Data.List.NonEmpty   (NonEmpty (..))

import qualified Avro.Deconflict.Reader as R
import qualified Avro.Deconflict.Writer as W
import qualified Data.Avro.Decode       as A (decodeAvro)
import qualified Data.Avro.Decode.Lazy  as AL
import qualified Data.Avro.Types        as Ty

import Test.Hspec

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

  it "should deconflict via strict containers" $ do
    w <- A.encodeContainer [[W.Outer "Peone" (W.Inner 3)]]

    let r = A.decodeContainer w :: [[R.Outer]]

    r `shouldBe` [[R.Outer "Peone" (R.Inner 3 Nothing)]]

  it "should deconflict via lazy containers" $ do
    w <- A.encodeContainer [[W.Outer "Peone" (W.Inner 3)]]

    let r = AL.decodeContainer w :: [Either String R.Outer]

    r `shouldBe` [Right (R.Outer "Peone" (R.Inner 3 Nothing))]
