{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Avro.DefaultsSpec
where

import qualified Data.Aeson          as J
import           Data.Avro
import           Data.Avro.Deriving
import           Data.Avro.Schema
import qualified Data.Avro.Types     as Ty
import qualified Data.HashMap.Strict as M
import qualified Data.Vector         as V

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/maybe.avsc"

spec :: Spec
spec = describe "Avro.DefaultsSpec: Schema with named types" $ do
  it "should decode value" $
    let msg = MaybeTest (Just "value")
    in fromAvro (toAvro msg) `shouldBe` pure msg

  it "should decode no value" $
    let msg = MaybeTest Nothing
    in fromAvro (toAvro msg) `shouldBe` pure msg

  it "should read default from Schema" $
    let
      msgSchema = schemaOf (undefined :: MaybeTest)
      fld = head (fields msgSchema)
      def = fldDefault fld
    in def `shouldNotBe` Nothing

  it "should encode schema with default" $
    let
      msgSchema = schemaOf (undefined :: MaybeTest)
      (J.Object jSchema) = J.toJSON msgSchema
      (Just (J.Array flds)) = M.lookup "fields" jSchema
      (J.Object jFld) = V.head flds
    in M.lookup "default" jFld `shouldBe` Just J.Null
