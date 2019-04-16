{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Avro.DeconflictSpec where

import Control.Monad.IO.Class
import Data.Avro              as A
import Data.Avro.Deconflict
import Data.Avro.Deriving
import Data.Avro.Schema
import Data.Either
import Data.List.NonEmpty     (NonEmpty (..))

import qualified Avro.Deconflict.A.Reader         as AR
import qualified Avro.Deconflict.A.Writer         as AW
import qualified Avro.Deconflict.B.Reader         as BR
import qualified Avro.Deconflict.B.Writer         as BW
import qualified Data.Avro.Decode                 as A (decodeAvro)
import qualified Data.Avro.Decode.Lazy            as AL
import qualified Data.Avro.Decode.Lazy.Deconflict as AL
import qualified Data.Avro.Deconflict             as A
import qualified Data.Avro.Types                  as Ty

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.DeconflictSpec" $ do
  describe "Type A" $ do
    it "should deconflict simple message" $ do
      let payload = A.encode $ AW.Inner 3
      let Right decodedAvro = A.decodeAvro AW.schema'Inner payload
      let Right deconflicted = deconflict AW.schema'Inner AR.schema'Inner decodedAvro
      fromAvro deconflicted `shouldBe` Success (AR.Inner 3 Nothing)

    it "should deconflict nested message" $ do
      let payload = A.encode AW.sampleValue
      let Right decodedAvro = A.decodeAvro AW.schema'Outer payload
      let Right deconflicted = deconflict AW.schema'Outer AR.schema'Outer decodedAvro

      fromAvro deconflicted `shouldBe` Success AR.sampleValue

    it "should deconflict strict container" $ do
      w <- A.encodeContainer [[AW.sampleValue]]
      A.decodeContainer w `shouldBe` [[AR.sampleValue]]

    it "should deconflict lazy container" $ do
      w <- A.encodeContainer [[AW.sampleValue]]
      AL.decodeContainer w `shouldBe` [Right AR.sampleValue]

    it "should deconflict lazy value" $ do
      let payload = A.encode AW.sampleValue
      let decodedAvro = AL.decodeAvro AW.schema'Outer payload
      let deconflicted = AL.deconflict AW.schema'Outer AR.schema'Outer decodedAvro

      AL.fromLazyAvro deconflicted `shouldBe` Success AR.sampleValue

    it "should deconflict strict value" $ do
      let payload = A.encode AW.sampleValue
      let Right decodedAvro = A.decodeAvro AW.schema'Outer payload
      let Right deconflicted = A.deconflict AW.schema'Outer AR.schema'Outer decodedAvro

      A.fromAvro deconflicted `shouldBe` Success AR.sampleValue


  describe "Type B" $ do
    it "should deconflict complex type" $ do
      let payload = A.encode BW.sampleValue
      let decodedAvro = AL.decodeAvro BW.schema'Foo payload
      let res = AL.deconflict BW.schema'Foo BR.schema'Foo decodedAvro

      AL.fromLazyAvro res `shouldBe` Success BR.sampleValue

    it "should deconflict lazy container" $ do
      w <- liftIO $ A.encodeContainer [[ BW.sampleValue ]]
      AL.decodeContainer w `shouldBe` [ Right BR.sampleValue ]

    it "should deconflict lazy value" $ do
      let payload = A.encode AW.sampleValue
      let decodedAvro = AL.decodeAvro AW.schema'Outer payload
      let deconflicted = AL.deconflict AW.schema'Outer AR.schema'Outer decodedAvro

      AL.fromLazyAvro deconflicted `shouldBe` Success AR.sampleValue

    it "should deconflict strict container" $ do
      w <- A.encodeContainer [[BW.sampleValue]]
      A.decodeContainer w `shouldBe` [[BR.sampleValue]]

    it "should deconflict strict value" $ do
      let payload = A.encode BW.sampleValue
      let Right decodedAvro = A.decodeAvro BW.schema'Foo payload
      let Right deconflicted = A.deconflict BW.schema'Foo BR.schema'Foo decodedAvro

      A.fromAvro deconflicted `shouldBe` Success BR.sampleValue
