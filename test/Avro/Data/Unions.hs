{-# LANGUAGE DataKinds           #-}
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
import Data.Avro.Sum
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
  a <- Gen.choice [makeNSum <$> txtGen, makeNSum <$> Gen.int64 Range.linearBounded]
  b <- Gen.maybe (Gen.int32 Range.linearBounded)
  c <- Gen.choice [makeNSum <$> fooGen, makeNSum <$> barGen]
  d <- Gen.choice [makeNSum <$> fooGen, makeNSum <$> notFooGen]
  e <- Gen.choice
        [ makeNSum <$> Gen.list (Range.linear 0 10) txtGen
        , makeNSum <$> Gen.map (Range.linear 0 10) (tupleGen txtGen (Gen.int64 Range.linearBounded))
        ]
  f <- makeNSum <$> Gen.int32 Range.linearBounded
  g <- Gen.choice
        [ makeNSum <$> Gen.int32 Range.linearBounded
        , makeNSum <$> txtGen
        , makeNSum <$> Gen.int64 Range.linearBounded
        ]
  h <- Gen.choice
        [ makeNSum <$> Gen.int32 Range.linearBounded
        , makeNSum <$> txtGen
        , makeNSum <$> Gen.int64 Range.linearBounded
        , makeNSum <$> fooGen
        ]
  i <- Gen.choice
        [ makeNSum <$> Gen.int32 Range.linearBounded
        , makeNSum <$> txtGen
        , makeNSum <$> Gen.int64 Range.linearBounded
        , makeNSum <$> fooGen
        , makeNSum <$> notFooGen
        ]
  j <- Gen.choice
        [ makeNSum <$> Gen.int32 Range.linearBounded
        , makeNSum <$> txtGen
        , makeNSum <$> Gen.int64 Range.linearBounded
        , makeNSum <$> fooGen
        , makeNSum <$> notFooGen
        , makeNSum <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        ]
  k <- Gen.choice
        [ makeNSum <$> Gen.int32 Range.linearBounded
        , makeNSum <$> txtGen
        , makeNSum <$> Gen.int64 Range.linearBounded
        , makeNSum <$> fooGen
        , makeNSum <$> notFooGen
        , makeNSum <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        , makeNSum <$> Gen.bool
        ]
  l <- Gen.choice
        [ makeNSum <$> Gen.int32 Range.linearBounded
        , makeNSum <$> txtGen
        , makeNSum <$> Gen.int64 Range.linearBounded
        , makeNSum <$> fooGen
        , makeNSum <$> notFooGen
        , makeNSum <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        , makeNSum <$> Gen.bool
        , makeNSum <$> Gen.double (Range.linearFrac (-27000.0) 27000.0)
        ]
  m <- Gen.choice
        [ makeNSum <$> Gen.int32 Range.linearBounded
        , makeNSum <$> txtGen
        , makeNSum <$> Gen.int64 Range.linearBounded
        , makeNSum <$> fooGen
        , makeNSum <$> notFooGen
        , makeNSum <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        , makeNSum <$> Gen.bool
        , makeNSum <$> Gen.double (Range.linearFrac (-27000.0) 27000.0)
        , makeNSum <$> Gen.bytes (Range.linear 0 50)
        ]
  n <- Gen.choice
        [ makeNSum   <$> Gen.int32 Range.linearBounded
        , makeNSum   <$> txtGen
        , makeNSum   <$> Gen.int64 Range.linearBounded
        , makeNSum   <$> fooGen
        , makeNSum   <$> notFooGen
        , makeNSum   <$> Gen.float (Range.linearFrac (-27000.0) 27000.0)
        , makeNSum   <$> Gen.bool
        , makeNSum   <$> Gen.double (Range.linearFrac (-27000.0) 27000.0)
        , makeNSum   <$> Gen.bytes (Range.linear 0 50)
        , makeNSum  <$> barGen
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
