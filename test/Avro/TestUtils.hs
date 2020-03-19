module Avro.TestUtils
where

import Control.Monad               (join)
import Control.Monad.IO.Class      (MonadIO)
import Data.Avro                   (Codec, DecodeAvro, EncodeAvro, decodeContainerWithEmbeddedSchema, decodeValueWithSchema, encodeContainer, encodeValue, nullCodec)
import Data.Avro.Schema.ReadSchema (fromSchema)
import Data.Avro.Schema.Schema     (Schema)
import Data.ByteString.Lazy        (ByteString)

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import           Hedgehog.Range              (Range)
import qualified Hedgehog.Range              as Range

roundtrip :: (EncodeAvro a, DecodeAvro a) => Schema -> a -> Either String a
roundtrip sch a = decodeValueWithSchema (fromSchema sch) (encodeValue sch a)

roundtripContainer' :: (MonadIO m, Show a, Eq a, EncodeAvro a, DecodeAvro a) => Codec -> Schema -> [[a]] -> PropertyT m ()
roundtripContainer' codec sch as = do
  bs <- evalIO $ encodeContainer codec sch as
  decoded <- evalEither $ sequence $ decodeContainerWithEmbeddedSchema bs
  join as === decoded

roundtripContainer :: (MonadIO m, Show a, Eq a, EncodeAvro a, DecodeAvro a) => Schema -> [[a]] -> PropertyT m ()
roundtripContainer = roundtripContainer' nullCodec

roundtripGen :: (MonadIO m, Eq a, Show a, EncodeAvro a, DecodeAvro a) => Schema -> Gen a -> PropertyT m ()
roundtripGen sch gen = do
  value <- forAll gen
  tripping value (encodeValue sch) (decodeValueWithSchema (fromSchema sch))

roundtripContainerGen :: (MonadIO m, Eq a, Show a, EncodeAvro a, DecodeAvro a) => Schema -> Gen a -> PropertyT m ()
roundtripContainerGen s g = do
  let gList = Gen.list (Range.linear 1 5) g
  values <- forAll $ Gen.list (Range.linear 1 5) gList
  bs <- evalIO $ encodeContainer nullCodec s values
  decoded <- evalEither $ sequence $ decodeContainerWithEmbeddedSchema bs

  join values === decoded
