{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Avro.Data.Unions
where

import Data.Avro.Deriving (deriveAvroFromByteString)
import Data.Avro.EitherN
import Text.RawString.QQ

import           Hedgehog       (Gen, MonadGen)
import qualified Hedgehog.Gen   as Gen
import           Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range

deriveAvroFromByteString [r|
{ "type" : "record",
  "name" : "Unions",
  "namespace" : "haskell.avro.example",
  "fields" : [
    { "name" : "scalars",
      "type" : ["string", "long"],
      "default" : "foo"
    },
    { "name" : "nullable",
      "type" : ["null", "int"],
      "default" : null
    },
    { "name" : "records",
      "type" : [
        { "type" : "record",
          "name" : "Foo",
          "fields" : [
            { "name" : "stuff",
              "type" : "string"
            }
          ]
        },
        { "type" : "record",
          "name" : "Bar",
          "fields" : [
            { "name" : "stuff",
              "type" : "string"
            },
            { "name" : "things",
              "type" : "Foo"
            }
          ]
        }
      ]
    },
    { "name" : "sameFields",
      "type" : [
        "Foo",
        { "type" : "record",
          "name" : "NotFoo",
          "fields" : [
            { "name" : "stuff", "type" : "string" }
          ]
        }
      ]
    },
    { "name" : "arrayAndMap",
      "type" : [
        { "type" : "array",
          "items" : "string"
        },
        { "type" : "map",
          "values" : "long"
        }
      ]
    },
    { "name" : "three", "type" : ["int", "string", "long"] },
    { "name" : "four", "type" : ["int", "string", "long", "Foo"] },
    { "name" : "five", "type" : ["int", "string", "long", "Foo", "NotFoo"] }
  ]
}
|]

unionsGen :: MonadGen m => m Unions
unionsGen = do
  let txtGen = Gen.text (Range.linear 0 100) Gen.alphaNum
  a <- Gen.choice [Left <$> txtGen, Right <$> Gen.int64 Range.linearBounded]
  b <- Gen.maybe (Gen.int32 Range.linearBounded)
  c <- Gen.choice [Left <$> fooGen, Right <$> barGen]
  d <- Gen.choice [Left <$> fooGen, Right <$> notFooGen]
  e <- Gen.choice
        [ Left <$> Gen.list (Range.linear 0 50) txtGen
        , Right <$> Gen.map (Range.linear 0 128) (tupleGen txtGen (Gen.int64 Range.linearBounded))
        ]
  f <- Gen.choice
        [ E3_1 <$> Gen.int32 Range.linearBounded
        , E3_2 <$> txtGen
        , E3_3 <$> Gen.int64 Range.linearBounded
        ]
  g <- Gen.choice
        [ E4_1 <$> Gen.int32 Range.linearBounded
        , E4_2 <$> txtGen
        , E4_3 <$> Gen.int64 Range.linearBounded
        , E4_4 <$> fooGen
        ]
  h <- Gen.choice
        [ E5_1 <$> Gen.int32 Range.linearBounded
        , E5_2 <$> txtGen
        , E5_3 <$> Gen.int64 Range.linearBounded
        , E5_4 <$> fooGen
        , E5_5 <$> notFooGen
        ]
  pure $ Unions a b c d e f g h

fooGen :: MonadGen m => m Foo
fooGen = Foo <$> Gen.text (Range.linear 0 512) Gen.unicodeAll

barGen :: MonadGen m => m Bar
barGen = Bar
  <$> Gen.text (Range.linear 0 512) Gen.unicodeAll
  <*> fooGen

notFooGen :: MonadGen m => m NotFoo
notFooGen = NotFoo <$> Gen.text (Range.linear 0 512) Gen.unicodeAll

tupleGen :: MonadGen m => m a -> m b -> m (a, b)
tupleGen a b = (,) <$> a <*> b
