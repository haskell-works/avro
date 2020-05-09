{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Avro.Encoding.DeconflictSpec
where

import qualified Avro.Data.Deconflict.Read   as Read
import qualified Avro.Data.Deconflict.Write  as Write
import           Control.Lens
import           Data.Avro                   (decodeValueWithSchema, encodeValueWithSchema)
import           Data.Avro.Schema.Deconflict (deconflict)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy
import           Data.Generics.Product       (field)
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Avro.Encoding.DeconflictSpec" $ do
  describe "Deconflict between reader and writer" $ do
    it "should deconfict base scenario" $ require $ property $ do
      x       <- forAll Write.genFoo
      schema  <- evalEither $ deconflict Write.schema'Foo Read.schema'Foo

      footnoteShow schema

      let bs  = encodeValueWithSchema Write.schema'Foo x
      x'      <- evalEither $ decodeValueWithSchema @Read.Foo schema bs

      let bar  = x  ^. field @"fooFooBar"
      let bar' = x' ^. field @"fooFooBar"

      bar' ^. field @"barBarInt"      === bar ^. field @"barBarInt"
      bar' ^. field @"barBarTime"     === posixSecondsToUTCTime (realToFrac (bar ^. field @"barBarTime") / 1000)
      bar' ^. field @"barBarLong"     === posixSecondsToUTCTime (realToFrac (bar ^. field @"barBarLong") / 1000000)
      bar' ^. field @"barBarString"   === bar ^. field @"barBarString"
      bar' ^. field @"barBarMissing"  === 42.2

      bar' ^. field @"barBarMooMissing" === Read.Moo 42 2

      -- Default values are only considered if the writer doesn't have that field
      x ^. field @"fooFooOption" === x' ^. field @"fooFooOption"

