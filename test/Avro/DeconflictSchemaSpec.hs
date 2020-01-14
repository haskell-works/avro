{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Avro.DeconflictSchemaSpec where

import Control.Monad.IO.Class
import Data.Avro               as A
import Data.Avro.Deconflict
import Data.Avro.Deriving
import Data.Avro.Schema
import Data.ByteString.Builder
import Data.ByteString.Lazy
import Data.Either
import Data.List.NonEmpty      (NonEmpty (..))

import qualified Avro.Deconflict.A.Reader      as AR
import qualified Avro.Deconflict.A.Writer      as AW
import qualified Avro.Deconflict.B.Reader      as BR
import qualified Avro.Deconflict.B.Writer      as BW
import qualified Avro.Deconflict.C.Reader      as CR
import qualified Avro.Deconflict.C.Writer      as CW
import qualified Avro.Deconflict.D.Reader      as DR
import qualified Avro.Deconflict.D.Writer      as DW
import qualified Avro.Deconflict.Unions.Reader as UnionsR
import qualified Avro.Deconflict.Unions.Writer as UnionsW

import           Data.Avro.Encoding.FromEncoding (decodeValueWithSchema)
import qualified Data.Avro.Encoding.ToEncoding   as Encode
import qualified Data.Avro.Schema.Deconflict     as Schema

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.DeconflictSchemaSpec" $ do
  it "Unions" $ require $ property $ do
    resSchema <- evalEither $ Schema.deconflict' UnionsW.schema'Record UnionsR.schema'Record
    x <- forAll UnionsW.recordGen
    let nameField = UnionsW.recordName x
    let bs = toLazyByteString $ Encode.toEncoding UnionsW.schema'Record x
    let result = decodeValueWithSchema @UnionsR.Record resSchema bs

    let expected = UnionsR.Record (Just nameField)
    result === Right expected
