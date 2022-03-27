module Avro.Gen.Schema
where

import Data.Avro.Schema.Schema

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import           Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range

null :: MonadGen m => m Schema
null = pure Null

boolean :: MonadGen m => m Schema
boolean = pure Boolean

decimalGen :: MonadGen m => m Decimal
decimalGen = Decimal
  <$> Gen.integral (Range.linear 0 10)
  <*> Gen.integral (Range.linear 0 10)

int :: MonadGen m => m Schema
int = do
  dec <- decimalGen
  Int <$> Gen.maybe (Gen.element [DecimalI dec, Date, TimeMillis])

long :: MonadGen m => m Schema
long = do
  dec <- decimalGen
  Long <$> Gen.maybe (Gen.element
    [DecimalL dec, TimeMicros, TimestampMillis,
     TimestampMicros, LocalTimestampMillis, LocalTimestampMicros])
