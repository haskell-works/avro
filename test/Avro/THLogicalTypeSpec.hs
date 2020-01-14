{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.THLogicalTypeSpec
where

import Control.Lens
import Control.Monad

import qualified Data.Aeson      as J
import           Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.Char       as Char
import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as T

import Test.Hspec

import Data.Avro
import Data.Avro.Deriving
import Data.Avro.Internal.Time
import Data.Avro.Schema

deriveAvro "test/data/logical.avsc"

spec :: Spec
spec = describe "Avro.THSpec: Logical Type Schema" $ do
  let msgs =
        [ Logical (millisToUTCTime 12345)
        , Logical (millisToUTCTime 67890)
        ]

  it "should do roundtrip" $
    forM_ msgs $ \msg ->
      fromAvro (toAvro msg) `shouldBe` pure msg

  it "should do full round trip" $
    forM_ msgs $ \msg -> do
      let encoded = encode msg
      let decoded = decode encoded

      decoded `shouldBe` pure msg
