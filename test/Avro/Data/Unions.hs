{-# LANGUAGE DeriveAnyClass      #-}
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

import Data.Avro.Deriving    (deriveAvroFromByteString, r)
import Data.Avro.EitherN
import Data.Functor.Identity

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
    { "name" : "one", "type"    : ["int"] },
    { "name" : "three", "type"  : ["int", "string", "long"] },
    { "name" : "four", "type"   : ["int", "string", "long", "Foo"] },
    { "name" : "five", "type"   : ["int", "string", "long", "Foo", "NotFoo"] },
    { "name" : "six", "type"    : ["int", "string", "long", "Foo", "NotFoo", "float"] },
    { "name" : "seven", "type"  : ["int", "string", "long", "Foo", "NotFoo", "float", "boolean"] },
    { "name" : "eight", "type"  : ["int", "string", "long", "Foo", "NotFoo", "float", "boolean", "double"] },
    { "name" : "nine", "type"   : ["int", "string", "long", "Foo", "NotFoo", "float", "boolean", "double", "bytes"] },
    { "name" : "ten", "type"    : ["int", "string", "long", "Foo", "NotFoo", "float", "boolean", "double", "bytes", "Bar"] }
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
        [ Left  <$> Gen.list (Range.linear 0 10) txtGen
        , Right <$> Gen.map (Range.linear 0 10) (tupleGen txtGen (Gen.int64 Range.linearBounded))
        ]
  f <- Identity <$> Gen.int32 Range.linearBounded
  g <- Gen.choice
        [ E3_1 <$> Gen.int32 Range.linearBounded
        , E3_2 <$> txtGen
        , E3_3 <$> Gen.int64 Range.linearBounded
        ]
  h <- Gen.choice
        [ E4_1 <$> Gen.int32 Range.linearBounded
        , E4_2 <$> txtGen
        , E4_3 <$> Gen.int64 Range.linearBounded
        , E4_4 <$> fooGen
        ]
  i <- Gen.choice
        [ E5_1 <$> Gen.int32 Range.linearBounded
        , E5_2 <$> txtGen
        , E5_3 <$> Gen.int64 Range.linearBounded
        , E5_4 <$> fooGen
        , E5_5 <$> notFooGen
        ]
  j <- Gen.choice
        [ E6_1 <$> Gen.int32 Range.linearBounded
        , E6_2 <$> txtGen
        , E6_3 <$> Gen.int64 Range.linearBounded
        , E6_4 <$> fooGen
        , E6_5 <$> notFooGen
        , E6_6 <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        ]
  k <- Gen.choice
        [ E7_1 <$> Gen.int32 Range.linearBounded
        , E7_2 <$> txtGen
        , E7_3 <$> Gen.int64 Range.linearBounded
        , E7_4 <$> fooGen
        , E7_5 <$> notFooGen
        , E7_6 <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        , E7_7 <$> Gen.bool
        ]
  l <- Gen.choice
        [ E8_1 <$> Gen.int32 Range.linearBounded
        , E8_2 <$> txtGen
        , E8_3 <$> Gen.int64 Range.linearBounded
        , E8_4 <$> fooGen
        , E8_5 <$> notFooGen
        , E8_6 <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        , E8_7 <$> Gen.bool
        , E8_8 <$> Gen.double (Range.linearFrac (-27000.0) 27000.0)
        ]
  m <- Gen.choice
        [ E9_1 <$> Gen.int32 Range.linearBounded
        , E9_2 <$> txtGen
        , E9_3 <$> Gen.int64 Range.linearBounded
        , E9_4 <$> fooGen
        , E9_5 <$> notFooGen
        , E9_6 <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        , E9_7 <$> Gen.bool
        , E9_8 <$> Gen.double (Range.linearFrac (-27000.0) 27000.0)
        , E9_9 <$> Gen.bytes (Range.linear 0 50)
        ]
  n <- Gen.choice
        [ E10_1   <$> Gen.int32 Range.linearBounded
        , E10_2   <$> txtGen
        , E10_3   <$> Gen.int64 Range.linearBounded
        , E10_4   <$> fooGen
        , E10_5   <$> notFooGen
        , E10_6   <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        , E10_7   <$> Gen.bool
        , E10_8   <$> Gen.double (Range.linearFrac (-27000.0) 27000.0)
        , E10_9   <$> Gen.bytes (Range.linear 0 50)
        , E10_10  <$> barGen
        ]
  pure $ Unions a b c d e f g h i j k l m n

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
