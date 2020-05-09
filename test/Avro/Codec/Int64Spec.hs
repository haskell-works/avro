{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Avro.Codec.Int64Spec (spec) where

import           Data.Avro.Internal.Zig  (zig)
import           Data.Bits
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as BL
import           Data.Int
import           Data.List.Extra
import           Data.Word
import           Numeric                 (showHex)

import           Avro.TestUtils
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Hspec

import           Data.Avro               (encodeValueWithSchema)
import           Data.Avro.Deriving      (deriveAvroFromByteString, r)
import qualified Data.Avro.Schema.Schema as Schema

{- HLINT ignore "Redundant do"        -}

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "OnlyInt64",
  "namespace": "test.contract",
  "fields": [ {"name": "onlyInt64Value", "type": "long"} ]
}
|]

bitStringToWord8s :: String -> [Word8]
bitStringToWord8s = reverse . map (toWord . reverse) . chunksOf 8 . reverse . toBinary
  where toBinary :: String -> [Bool]
        toBinary ('1':xs) = True  : toBinary xs
        toBinary ('0':xs) = False : toBinary xs
        toBinary (_  :xs) = toBinary xs
        toBinary       [] = []
        toWord' :: Word8 -> [Bool] -> Word8
        toWord' n (True :bs) = toWord' ((n `shiftL` 1) .|. 1) bs
        toWord' n (False:bs) = toWord' ((n `shiftL` 1) .|. 0) bs
        toWord' n _          = n
        toWord = toWord' 0

spec :: Spec
spec = describe "Avro.Codec.Int64Spec" $ do
  let schema = Schema.Long Nothing
  it "Can encode 90071992547409917L correctly" $ require $ withTests 1 $ property $ do
    let expectedBuffer = BL.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
    let value = OnlyInt64 90071992547409917
    encodeValueWithSchema schema value === expectedBuffer

  it "Can decode 90071992547409917L correctly" $ require $ withTests 1 $ property $ do
    let buffer = BL.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
    let value = OnlyInt64 90071992547409917
    encodeValueWithSchema schema value === buffer

  it "Can decode encoded Int64 values" $ require $ property $ do
    roundtripGen schema (Gen.int64 Range.linearBounded)

  it "Can decode 129L" $ require $ withTests 1 $ property $ do
    let w = 129 :: Int64
    w' <- evalEither $ roundtrip schema w
    w === w'

  it "Can decode 36028797018963968 correctly" $ require $ withTests 1 $ property $ do
    let buffer = BL.pack (bitStringToWord8s "10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 00000001")
    let value = OnlyInt64 36028797018963968
    encodeValueWithSchema schema value === buffer

  it "bitStringToWord8s 00000000"                   $  bitStringToWord8s "00000000"                    `shouldBe` [0x00             ]
  it "bitStringToWord8s 00000001"                   $  bitStringToWord8s "00000001"                    `shouldBe` [0x01             ]
  it "bitStringToWord8s 01111111"                   $  bitStringToWord8s "01111111"                    `shouldBe` [0x7f             ]
  it "bitStringToWord8s 10000000 00000001"          $  bitStringToWord8s "10000000 00000001"           `shouldBe` [0x80, 0x01       ]
  it "bitStringToWord8s 10000001 00000001"          $  bitStringToWord8s "10000001 00000001"           `shouldBe` [0x81, 0x01       ]
  it "bitStringToWord8s 10000010 00000001"          $  bitStringToWord8s "10000010 00000001"           `shouldBe` [0x82, 0x01       ]
  it "bitStringToWord8s 11111111 01111111"          $  bitStringToWord8s "11111111 01111111"           `shouldBe` [0xff, 0x7f       ]
  it "bitStringToWord8s 10000000 10000000 00000001" $  bitStringToWord8s "10000000 10000000 00000001"  `shouldBe` [0x80, 0x80, 0x01 ]
  it "bitStringToWord8s 10000001 10000000 00000001" $  bitStringToWord8s "10000001 10000000 00000001"  `shouldBe` [0x81, 0x80, 0x01 ]
  it "bitStringToWord8s 10000001 10000000 00000000" $  bitStringToWord8s "10000001 10000000 00000000"  `shouldBe` [0x81, 0x80, 0x00 ]

  it "Can zig" $ require $ withTests 1 $ property $ do
    zig (          0 :: Int64)  === 0
    zig (         -1 :: Int64)  === 1
    zig (          1 :: Int64)  === 2
    zig (         -2 :: Int64)  === 3
    zig ( 2147483647 :: Int64)  === 4294967294
    zig (-2147483648 :: Int64)  === 4294967295
