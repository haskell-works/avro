{-# LANGUAGE ScopedTypeVariables #-}

module Avro.Codec.CodecRawSpec (spec) where

import           Data.Avro.Internal.DecodeRaw
import           Data.Avro.Internal.EncodeRaw
import           Data.Binary.Get
import           Data.ByteString.Builder
import           Data.Int
import           Data.Word
import           Test.Hspec
import qualified Test.QuickCheck              as Q

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Avro.Codec.CodecRawSpec" $ do
  it "codec raw round trip Int"     $ Q.property $ \(w :: Int)    -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
  it "codec raw round trip Int8"    $ Q.property $ \(w :: Int8)   -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
  it "codec raw round trip Int16"   $ Q.property $ \(w :: Int16)  -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
  it "codec raw round trip Int32"   $ Q.property $ \(w :: Int32)  -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
  it "codec raw round trip Int64"   $ Q.property $ \(w :: Int64)  -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
  it "codec raw round trip Word"    $ Q.property $ \(w :: Word)   -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
  it "codec raw round trip Word8"   $ Q.property $ \(w :: Word8)  -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
  it "codec raw round trip Word16"  $ Q.property $ \(w :: Word16) -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
  it "codec raw round trip Word32"  $ Q.property $ \(w :: Word32) -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
  it "codec raw round trip Word64"  $ Q.property $ \(w :: Word64) -> runGet decodeRaw (toLazyByteString (encodeRaw w)) == w
