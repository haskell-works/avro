{-# LANGUAGE DeriveGeneric       #-}
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
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.Vector         as V

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

deriveAvro "test/data/maybe.avsc"

spec :: Spec
spec = describe "Avro.DefaultsSpec: Schema with named types" $ do
  it "should decode value" $
    let msg = MaybeTest (Just "value") (FixedTag "\0\42\255") "\0\37\255"
    in fromAvro (toAvro msg) `shouldBe` pure msg

  it "should decode no value" $
    let msg = MaybeTest Nothing (FixedTag "\0\42\255") "\0\37\255"
    in fromAvro (toAvro msg) `shouldBe` pure msg

  it "should read default from Schema" $
    let
      msgSchema = schemaOf (undefined :: MaybeTest)
      fixedSchema = schemaOf (undefined :: FixedTag)
      defaults = fldDefault <$> fields msgSchema
    in defaults `shouldBe` [ Just $ Ty.Union (V.fromList [Null, String]) Null Ty.Null
                           , Just $ Ty.Fixed fixedSchema "\0\42\255"
                           , Just $ Ty.Bytes "\0\37\255"
                           ]

  it "should encode schema with default" $
    let
      msgSchema = schemaOf (undefined :: MaybeTest)
      (J.Object jSchema) = J.toJSON msgSchema
      (Just (J.Array flds)) = M.lookup "fields" jSchema
      (J.Object jFld) = V.head flds
    in M.lookup "default" jFld `shouldBe` Just J.Null
