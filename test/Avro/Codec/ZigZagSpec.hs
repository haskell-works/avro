{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.ZigZagSpec (spec) where

import Data.Avro.Internal.Zag
import Data.Avro.Internal.Zig
import Data.Int
import Data.Word
import Test.Hspec
import Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.Codec.Int64Spec" $ do
  it "Zig and zag roundtrips for Int"     $ property $ \(w :: Int   ) -> zag (zig w) `shouldBe` w
  it "Zig and zag roundtrips for Int8"    $ property $ \(w :: Int8  ) -> zag (zig w) `shouldBe` w
  it "Zig and zag roundtrips for Int16"   $ property $ \(w :: Int16 ) -> zag (zig w) `shouldBe` w
  it "Zig and zag roundtrips for Int32"   $ property $ \(w :: Int32 ) -> zag (zig w) `shouldBe` w
  it "Zig and zag roundtrips for Int64"   $ property $ \(w :: Int64 ) -> zag (zig w) `shouldBe` w
  it "Zag and zig roundtrips for Word"    $ property $ \(w :: Word  ) -> zig (zag w) `shouldBe` w
  it "Zag and zig roundtrips for Word8"   $ property $ \(w :: Word8 ) -> zig (zag w) `shouldBe` w
  it "Zag and zig roundtrips for Word16"  $ property $ \(w :: Word16) -> zig (zag w) `shouldBe` w
  it "Zag and zig roundtrips for Word32"  $ property $ \(w :: Word32) -> zig (zag w) `shouldBe` w
  it "Zag and zig roundtrips for Word64"  $ property $ \(w :: Word64) -> zig (zag w) `shouldBe` w
