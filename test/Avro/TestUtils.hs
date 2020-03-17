module Avro.TestUtils
where

import Control.Monad                 (join)
import Control.Monad.IO.Class        (MonadIO)
import Data.Avro.Codec               (nullCodec)
import Data.Avro.Encoding.DecodeAvro
import Data.Avro.Encoding.EncodeAvro
import Data.Avro.Encoding.Value
import Data.Avro.Schema              (Schema)
import Data.Avro.Schema.ReadSchema   (fromSchema)

import qualified Data.Avro.Encoding.Container as Encoding


import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Hedgehog.Range              (Range)
import qualified Hedgehog.Range              as Range

roundtrip :: (EncodeAvro a, DecodeAvro a) => Schema -> a -> Either String a
roundtrip sch a = decodeValueWithSchema (fromSchema sch) (encodeAvro sch a)

roundtripGen :: (MonadIO m, Eq a, Show a, EncodeAvro a, DecodeAvro a) => Schema -> Gen a -> PropertyT m ()
roundtripGen sch gen = do
  value <- forAll gen
  tripping value (encodeAvro sch) (decodeValueWithSchema (fromSchema sch))

roundtripContainerGen :: (MonadIO m, Eq a, Show a, EncodeAvro a, DecodeAvro a) => Schema -> Gen a -> PropertyT m ()
roundtripContainerGen s g = do
  let gList = Gen.list (Range.linear 1 5) g
  values <- forAll $ Gen.list (Range.linear 1 5) gList
  bs <- evalIO $ Encoding.encodeContainer nullCodec s values
  decoded <- evalEither $ sequence $ Encoding.decodeContainerWithEmbeddedSchema bs

  join values === decoded
